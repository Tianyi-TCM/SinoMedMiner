# app.R
# 2025.2.7

options(shiny.maxRequestSize = 100 * 1024^2) # 设置最大上传文件大小为100MB
library(shiny)
library(readxl)
library(openxlsx)
library(DT)
library(SinoMedminer)
library(stringr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(colourpicker)
library(RColorBrewer)
library(shinyjs)
library(shinythemes)
library(bslib)
library(ggraph)
library(shinycssloaders)
library(shinyWidgets)
custom_css <- "

.btn-group-custom {
  margin-bottom: 10px;
}
.btn-space {
  margin-right: 10px;
}
.btn-custom-width {
  width: 120px; /* 设置按钮宽度为120px */
}
.separator {
  border-top: 1px solid #ccc;
  margin: 20px 0;
}
.file-input-margin {
  margin-bottom: 5px; /* 缩小文件输入框和下一个元素之间的距离 */
}
.param-group {
  display: flex;
  align-items: center;
  gap: 10px; /* 调整控件之间的间距 */
}
.tab-content {
  margin-top: 10px; /* 调整tab内容与标签页选项之间的间距 */
}
.download-options {
  display: flex;
  align-items: center;
  gap: 10px;
  margin-top: 10px;
}
"
# UI部分
ui <- shinyUI(fluidPage(
  tags$head(tags$style(HTML(custom_css))), # 添加自定义 CSS 样式
  useShinyjs(), # 启用ShinyJS功能
  theme = bs_theme(bootswatch = "sandstone"),
  navbarPage(

    title = "SinoMedminer",

    ###匹配字段####
    tabPanel("匹配字段",
             sidebarLayout(
               sidebarPanel(
                 fileInput("file", "上传文件", accept = ".xlsx", buttonLabel = "浏览",
                           placeholder = "请选择.xlsx文件"),
                 radioButtons("type", "选择数据清洗类型:",
                              choices = list("中文" = "Chinese", "英文" = "letters"),
                              selected = "Chinese"),
                 div(class = "btn-group-custom", # 使用自定义样式类
                     actionButton("clear", "清除", class = "btn btn-warning btn-space"),
                     downloadButton("downloadData", "匹配", class = "btn btn-success"),
                     downloadButton("downloadFreq", "频数", class = "btn btn-success") # 移动频数按钮到侧边栏
                 )
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("匹配", dataTableOutput("table")), # 匹配字段子页面
                   tabPanel("频数分析", dataTableOutput("freq_table"))
                 )
               )
             )
    ),
    #### 数据清洗 ####
    tabPanel("数据清洗",
             sidebarLayout(
               sidebarPanel(
                 fileInput("dataFile", "待清洗数据:", accept = ".xlsx", buttonLabel = "浏览",placeholder = ".xlsx文件"),
                 fileInput("lookupFile", "清洗规则:", accept = ".xlsx", buttonLabel = "浏览",placeholder = ".xlsx文件"),
                 div(class = "btn-group-custom", # 使用自定义样式类
                     actionButton("cleanData", "清洗数据", class = "btn btn-primary btn-space"),
                     downloadButton("downloadCleanedData", "清洗后的数据", class = "btn btn-success")
                 ),
                 tags$hr(),
                 h3("数据转换"),
                 div(class = "btn-group-custom", # 使用自定义样式类
                     actionButton("convertToWide", "转换为宽格式", class = "btn btn-info btn-space"),
                     actionButton("convertToLong", "转换为长格式", class = "btn btn-info")
                 ),
                 div(class = "btn-group-custom", # 使用自定义样式类
                     downloadButton("downloadWideData", "宽数据", class = "btn btn-success btn-space"),
                     downloadButton("downloadLongData", "长数据", class = "btn btn-success")
                 ),
                 div(id = "warning-message", class = "alert alert-warning", style = "display: none;") # 添加用于显示警告信息的div
               ),
               mainPanel(
                 dataTableOutput("resultTable")
               )
             )
    ),
    #### 中药属性####
    # tags$head(tags$style(HTML(custom_css))), # 添加自定义 CSS 样式
    tabPanel("中药属性",
             sidebarLayout(
               sidebarPanel(
                 useShinyjs(),
                 fileInput("propertyFile", "宽格式数据", accept = ".xlsx", buttonLabel = "浏览",placeholder = ".xlsx文件"),
                 fileInput("additionalPropertyFile", "追加中药属性", accept = ".xlsx", buttonLabel = "浏览",,placeholder = ".xlsx文件"),  # 新增的文件上传控件
                 fileInput("efficacyFile", "追加中药功效", accept = ".xlsx", buttonLabel = "浏览",placeholder = ".xlsx文件"),  # 新增的功效文件上传控件
                 div(class = "btn-group-custom",
                     actionButton("calcProperty", "统计分析", class = "btn btn-primary btn-space btn-custom-width"),
                     downloadButton("downloadProperty", "统计结果", class = "btn btn-success btn-custom-width")
                 ),
                 div(class = "btn-group-custom",
                     actionButton("toggle_visual_params", "属性可视化", class = "btn btn-info btn-space btn-custom-width"),
                     actionButton("toggle_efficacy_params", "功效可视化", class = "btn btn-info btn-custom-width")
                 ),
                 hidden(
                   div(id = "visual_params",
                       colourpicker::colourInput("line_col", "线条颜色", value = "#000000"),
                       colourpicker::colourInput("bac_col", "背景颜色", value = "#FFFFFF"),
                       numericInput("label_size", "字体比例", value = 2.5, min = 0.5,step = 0.1),
                       numericInput("line_width", "线条粗细", value = 1, min = 0.1, step = 0.1)
                   )
                 ),
                 hidden(
                   div(id = "efficacy_params",
                       selectInput("cal_pal", "配色",
                                   choices = c("Set1", "Set2", "Set3", "Paired", "Accent", "Dark2", "Pastel1", "Pastel2"),
                                   selected = "Set3"),
                       numericInput("ignore", "ignore参数", value = 0.03, min = 0.01, max = 0.99, step = 0.01),
                       numericInput("lab_size", "字体比例", value = 2, min = 0.1, step = 0.1)
                   )
                 ),
                 selectInput("file_format", "保存文件格式",
                             choices = c("PDF", "TIFF"),
                             selected = "TIFF"),
                 numericInput("img_width_efficacy", "宽度 (cm)", value = 8, min = 1),
                 numericInput("img_height_efficacy", "高度 (cm)", value = 6, min = 1)
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("归经",
                            dataTableOutput("propertyGjTable")
                   ),
                   tabPanel("四气",
                            dataTableOutput("propertySqTable")
                   ),
                   tabPanel("五味",
                            dataTableOutput("propertyTasteTable")
                   ),
                   tabPanel("功效",
                            dataTableOutput("efficacyTable")
                   ),
                   tabPanel("归经可视化",
                            plotOutput("plotGjVisualization"),
                            downloadButton("downloadGjPlot", "图片", class = "btn btn-warning btn-custom-width")
                   ),
                   tabPanel("四气可视化",
                            plotOutput("plotSqVisualization"),
                            downloadButton("downloadSqPlot", "图片", class = "btn btn-warning btn-custom-width")
                   ),
                   tabPanel("五味可视化",
                            plotOutput("plotTasteVisualization"),
                            downloadButton("downloadTastePlot", "图片", class = "btn btn-warning btn-custom-width")
                   ),
                   tabPanel("功效可视化",
                            plotOutput("plotEfficacyVisualization"),
                            downloadButton("downloadEfficacyPlot", "图片", class = "btn btn-warning btn-custom-width")
                   )
                 )
               )
             )
    ),

    #### 关联规则 ####
    tabPanel("关联规则",
             sidebarLayout(
               sidebarPanel(
                 h4("探索规则"),
                 div(class = "file-input-margin",
                     fileInput("arFile", "上传您的.xlsx文件", accept = ".xlsx", buttonLabel = "浏览")
                 ),
                 textInput("supp_values", "输入支持度值 (逗号分隔)", value = "0.05, 0.07, 0.10, 0.12, 0.15, 0.2, 0.22, 0.25, 0.28, 0.3"),
                 textInput("conf_values", "输入置信度值 (逗号分隔)", value = "0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9"),
                 div(class = "btn-group-custom",
                     actionButton("exploreRules", "探索规则", class = "btn btn-primary"),
                     downloadButton("downloadExpRules", "探索规则表", class = "btn btn-success btn-space")
                 ),
                 div(class = "separator"),
                 h4("分析规则"),
                 numericInput("supp", "输入支持度值", value = 0.07, min = 0, max = 1, step = 0.01),
                 numericInput("conf", "输入置信度值", value = 0.6, min = 0, max = 1, step = 0.01),
                 numericInput("min_lift", "输入最小提升度值", value = 1, min = 1, step = 0.1),
                 actionButton("analyzeRules", "分析规则", class = "btn btn-primary"),
                 downloadButton("downloadRules", "关联规则表", class = "btn btn-success")
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("规则探索",
                            div(id = "analyzing", "正在分析...", style = "color: red; display: none;"),
                            dataTableOutput("rulesTable")
                   ),
                   tabPanel("探索可视化",
                            div(class = "tab-content",
                                actionButton("toggle_params", "可视化", class = "btn btn-info btn-space btn-custom-width"),
                                hidden(
                                  div(id = "params",
                                      selectInput("col_pal", "选择配色方案", choices = c("A", "B", "C", "D", "E", "F", "G", "H"), selected = "D")
                                  )
                                ),
                                style = "display: flex; align-items: center;"
                            ),
                            plotOutput("expRulesPlot"),
                            fluidRow(
                              column(2, downloadButton("downloadExpPlot", "图片", class = "btn btn-success"),
                                     style = "margin-bottom: 15px;"),
                              column(3, selectInput("expPlotType", "图片类型", choices = c("PDF", "TIFF"), selected = "TIFF")),
                              column(3,numericInput("expPlotWidth", "宽度 (cm)", value = 8, min = 1)),
                              column(3,numericInput("expPlotHeight", "高度 (cm)", value = 6, min = 1)),
                              style = "display: flex; align-items: flex-end; gap: 5px;")

                   ),
                   tabPanel("分析规则",
                            dataTableOutput("analyzedRulesTable")
                   ),
                   tabPanel("规则分布",
                            div(class = "tab-content param-group",
                                selectInput("distr_col_pal", "选择配色方案", choices = c("A", "B", "C", "D", "E", "F", "G", "H"), selected = "D"),
                                numericInput("distr_top", "提取百分比", value = 1, min = 0, step = 0.01)
                            ),
                            plotOutput("rulesDistrPlot"),

                            fluidRow(
                              column(2, downloadButton("downloadDistrPlot", "图片", class = "btn btn-success"),
                                     style = "margin-bottom: 15px;"),
                              column(3, selectInput("distrPlotType", "图片类型", choices = c("PDF", "TIFF"), selected = "TIFF")),
                              column(3,numericInput("distrPlotWidth", "图片宽度 (cm)", value = 8, min = 1)),
                              column(3,numericInput("distrPlotHeight", "图片高度 (cm)", value = 6, min = 1)),
                              style = "display: flex; align-items: flex-end; gap: 5px;")
                   ),
                   tabPanel("规则矩阵",
                            fluidRow(
                              column(2, selectInput("shading", "颜色映射", choices = c("提升度", "置信度", "支持度"), selected = "提升度")),
                              column(2, selectInput("measure", "大小映射", choices = c("提升度", "置信度", "支持度"), selected = "支持度")),
                              column(2, colourpicker::colourInput("col_max", "最大值", value = "#EEA2AD")),
                              column(2, colourpicker::colourInput("col_min", "最小值", value = "#8DB6CD")),
                              column(2, numericInput("alpha", "透明度", value = 0.7, min = 0, max = 1, step = 0.01)),
                              column(2, numericInput("k_rules", "规则数", value = 20, min = 1, step = 1))
                            ),
                            plotOutput("rulesMatrixPlot"),

                            fluidRow(
                              column(2, downloadButton("downloadMatrixPlot", "图片", class = "btn btn-success"),
                                     style = "margin-bottom: 15px;"),
                              column(3, selectInput("matrixPlotType", "图片类型", choices = c("PDF", "TIFF"), selected = "TIFF")),
                              column(3,numericInput("matrixPlotWidth", "图片宽度 (cm)", value = 8, min = 1)),
                              column(3,numericInput("matrixPlotHeight", "图片高度 (cm)", value = 6, min = 1)),
                              style = "display: flex; align-items: flex-end; gap: 5px;")
                   ),
                   tabPanel("规则网络",
                            fluidRow(
                              column(2, selectInput("rule_size", "大小映射", choices = c("支持度", "置信度", "提升度"), selected = "支持度")),
                              column(2, selectInput("rule_color", "颜色映射", choices = c("支持度", "置信度", "提升度"), selected = "置信度")),
                              column(2, colourpicker::colourInput("min_val", "最小值", value = "#FFDEAD")),
                              column(2, colourpicker::colourInput("max_val", "最大值", value = "blue")),
                              column(2, colourpicker::colourInput("edge_col", "连线色", value = "blue")),
                              column(2, numericInput("num_rules", "规则数", value = 10, min = 1, step = 1))
                            ),
                            plotOutput("rulesNetworkPlot"),

                            fluidRow(
                              column(2, downloadButton("downloadNetworkPlot", "图片", class = "btn btn-success"),
                                     style = "margin-bottom: 15px;"),
                              column(3, selectInput("networkPlotType", "图片类型", choices = c("PDF", "TIFF"), selected = "TIFF")),
                              column(3,numericInput("networkPlotWidth", "图片宽度 (cm)", value = 8, min = 1)),
                              column(3,numericInput("networkPlotHeight", "图片高度 (cm)", value = 6, min = 1)),
                              style = "display: flex; align-items: flex-end; gap: 5px;")
                   )
                 )
               )
             )
    ),
    ##### 处方相似度 #####
    tabPanel("处方相似度",
             sidebarLayout(
               sidebarPanel(
                 fileInput("simFile", "上传您的.xlsx文件", accept = ".xlsx", buttonLabel = "浏览"),
                 numericInput("index", "相似度阈值", value = 0.75, min = 0, max = 1),
                 actionButton("analyzeSim", "计算相似度", class = "btn btn-primary"),
                 downloadButton("downloadSimData", "相似度数据", class = "btn btn-success btn-space")
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("处方相似度",
                            dataTableOutput("simMatrixTable")
                   ),
                   tabPanel("可视化",
                            fluidRow(
                              column(6, selectInput("jaccard_colpal", "颜色方案",
                                                    choices = c("BrBG", "PiYG", "PRGn", "PuOr", "RdGy", "RdBu",
                                                                "RdYlBu", "RdYlGn", "Spectral"),
                                                    selected = "RdYlBu")),
                              column(6, numericInput("cell_size", "单元大小", value = 2, min = 1))
                            ),
                            plotOutput("simHeatmap"),

                            fluidRow(
                              column(2, downloadButton("downloadHeatmap", "图片", class = "btn btn-success btn-space"),
                                     style = "margin-bottom: 15px;"),
                              column(3, selectInput("heatmapType", "图片类型", choices = c("PDF", "TIFF"), selected = "TIFF")),
                              column(3,numericInput("heatmapWidth", "图片宽度 (cm)", value = 8, min = 1)),
                              column(3,numericInput("heatmapHeight", "图片高度 (cm)", value = 6, min = 1)),
                              style = "display: flex; align-items: flex-end; gap: 5px;")
                   )
                 )
               )
             )
    ), ##### Phi #####
    tabPanel("聚类分析",
             sidebarLayout(
               sidebarPanel(
                 fileInput("clusterFile", "上传您的.xlsx文件", accept = ".xlsx", buttonLabel = "浏览"),
                 numericInput("top_phi", "提取前n个字段", value = 30, min = 2),
                 numericInput("k_phi", "聚类数", value = 5, min = 2),
                 actionButton("analyzePhi", "Phi相关分析", class = "btn btn-primary")
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Phi相关性",
                            dataTableOutput("phiMatrixTable")
                   ),
                   tabPanel("Phi聚类可视化",
                            fluidRow(
                              column(2, selectInput("phi_cluster_colpal", "颜色方案",
                                                    choices = c("BrBG", "PiYG", "PRGn", "PuOr", "RdGy", "RdBu",
                                                                "RdYlBu", "RdYlGn", "Spectral"),
                                                    selected = "RdBu")),
                              column(2, numericInput("fontsize", "字体", value = 8, min = 1)),
                              column(2, colourpicker::colourInput("border_color", "单元边框", value = "#8B0A50")),
                              column(2, numericInput("cell_cize", "单格大小", value = 8, min = 1)),
                              column(2, numericInput("font_angle", "字体角度", value = 270, min = 0, max = 360)),
                              column(2, numericInput("tree_height", "聚类高度", value = 15, min = 1))
                            ),
                            plotOutput("phiClusterPlot"),
                            fluidRow(
                              column(2, downloadButton("downloadPhiPlot", "图片", class = "btn btn-success btn-space"),
                                     style = "margin-bottom: 15px;"),
                              column(3, selectInput("phiPlotType", "图片类型", choices = c("PDF", "TIFF"), selected = "TIFF")),
                              column(3,numericInput("phiPlotWidth", "图片宽度 (cm)", value = 8, min = 1)),
                              column(3,numericInput("phiPlotHeight", "图片高度 (cm)", value = 6, min = 1)),
                              style = "display: flex; align-items: flex-end; gap: 5px;")
                   )
                 )
               )
             )
    ), ##### 共现网络 ######
    tabPanel("共现网络",
             sidebarLayout(
               sidebarPanel(
                 fileInput("coocFile", "上传Excel文件", accept = ".xlsx", buttonLabel = "浏览",placeholder = "选择一个.xlsx文件"),
                 numericInput("top_n", "提取前n个字段", value = 30, min = 2),
                 numericInput("min_threshold", "最小共现次数", value = 2, min = 1),
                 actionButton("generateCooc", "生成共现网络", class = "btn btn-primary"),
                 downloadButton("downloadCoocNet", "下载数据", class = "btn btn-success")
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("节点",
                            dataTableOutput("nodesTable")
                   ),
                   tabPanel("边",
                            dataTableOutput("edgesTable")
                   ),
                   tabPanel("可视化",
                            fluidRow(
                              column(2, selectInput("node_layout", "布局",
                                                    choices = c("fr", "circle", "kk", "gem", "randomly", "nicely", "stress", "grid", "dh", "star", "graphopt", "sugiyama", "sphere"),
                                                    selected = "fr")),
                              column(2, selectInput("line_type", "线型", choices = c("arc", "straight"), selected = "arc")),
                              column(2, numericInput("text_size_cooc", "字号", value = 5, min = 1, step = 0.5)),
                              column(2, sliderInput("node_size", "节点大小", min = 2, max = 16, value = c(4, 10), step = 2)),
                              column(2, sliderInput("edge_width", "线宽", min = 0, max = 1.5, value = c(0.3, 0.6), step = 0.1))
                            ),

                            fluidRow(
                              column(2, colourpicker::colourInput("font_color", "字体颜色", value = "black")),
                              column(2, colourpicker::colourInput("edge_col_low", "共现最低", value = "steelblue")),
                              column(2, colourpicker::colourInput("edge_col_high", "共现最高", value = "tomato")),
                              column(2, numericInput("node_alpha", "节点透明", value = 0.55, min = 0, max = 1, step = 0.05)),
                              column(2, selectInput("col_pal_cooc", "节点配色",
                                                    choices = c("Accent", "Dark2", "Paired", "Pastel1", "Pastel2", "Set1", "Set2", "Set3"),
                                                    selected = "Pastel1"))
                            ),
                            plotOutput("coocPlot", height = "600px"),

                            fluidRow(
                              column(2, downloadButton("downloadCoocPlot", "图片", class = "btn btn-success"),
                                     style = "margin-bottom: 15px;"),
                              column(3, selectInput("plotFormat", "图片类型", choices = c("TIFF", "PDF"), selected = "TIFF")),
                              column(3,numericInput("plotWidth", "图片宽度 (cm)", value = 8, min = 1)),
                              column(3,numericInput("plotHeight", "图片高度 (cm)", value = 6, min = 1)),
                              style = "display: flex; align-items: flex-end; gap: 5px;")
                   )
                 )
               )
             )
    ),
    ### 剂量分析 ####
    tabPanel("剂量分析",

             sidebarLayout(

               # 侧边栏：用于上传文件和按钮
               sidebarPanel(
                 fileInput("file1", "上传Excel文件", accept = c(".xlsx")),
                 hr(),
                 actionButton("analyze", "分析",class = "btn btn-primary"), # 分析按钮
                 downloadButton("download_table", "下载表格",class = "btn btn-success") # 下载按钮
               ),

               # 主区域：剂量分布表和剂量分布图
               mainPanel(
                 tabsetPanel(
                   ### 剂量分布表 ###
                   tabPanel("剂量分布表",
                            DTOutput("dose_table") # 使用DT包显示表格
                   ),

                   ### 剂量分布图 ###
                   tabPanel("剂量分布图",
                            # 参数选择位于图表上方
                            fluidRow(
                              column(2, numericInput("top_dose", "高频药物", value = 25, min = 1)),
                              column(2, numericInput("percent_ingore", "百分比最低", value = 5, min = 0)),
                              column(2, numericInput("bar_line_size", "外框粗细", value = 0.5, step = 0.1)),
                              column(2, numericInput("bar_alpha", "柱子透明度", value = 0.7, min = 0, max = 1, step = 0.1)),
                              column(2, numericInput("text_size_dose", "字体比例", value = 1.5, step = 0.1))
                            ),
                            fluidRow(
                              column(2, selectInput("dose_colpal", "柱子颜色",
                                                    choices = c("Accent", "Dark2", "Paired", "Pastel1",
                                                                "Pastel2", "Set1", "Set2", "Set3"),
                                                    selected = "Set1")),
                              column(2, colourpicker::colourInput("min_col", "最小值", value = "steelblue")), # 从colourpicker包调用
                              column(2, colourpicker::colourInput("max_col", "最大值", value = "salmon")), # 从colourpicker包调用
                              column(2, colourpicker::colourInput("mean_col", "平均值", value = "darkred")), # 从colourpicker包调用
                              column(2, colourpicker::colourInput("dumbbell_col", "连线", value = "gray60")) # 从colourpicker包调用
                            ),

                            # 图表输出
                            plotOutput("dose_plot"),

                            # 图片下载设置和按钮
                            hr(),
                            fluidRow(
                              column(2, style = "display: flex; align-items: flex-end; margin-bottom: 16px;",
                                     downloadButton("download_plot_dose", "图片", class = "btn btn-success")),
                              column(3, style = "display: flex; align-items: flex-end",
                                     selectInput("img_format_dose", "选择格式", choices = c("pdf", "tiff"), selected = "tiff")),
                              column(3, style = "display: flex; align-items: flex-end",
                                     numericInput("img_width_dose", "宽度（cm）", value = 8, min = 1)),
                              column(3, style = "display: flex; align-items: flex-end",
                                     numericInput("img_height_dose", "高度（cm）", value = 6, min = 1))
                            )

                   )
                 )
               )
             )
    ),
    #### 综合分析 ####
    tabPanel("综合分析",
             sidebarLayout(
               sidebarPanel(
                 fileInput("file1_sankey", "维度1数据", accept = c(".xlsx"), placeholder = ".xlsx文件"),
                 fileInput("file2_sankey", "维度2数据", accept = c(".xlsx"), placeholder = ".xlsx文件"),

                 # 动态UI输出
                 uiOutput("file3_ui"),
                 uiOutput("file4_ui"),

                 hr(),

                 actionButton("generate_sankey", "生成结果", class = "btn btn-primary")
               ),

               mainPanel(
                 fluidRow(
                   column(3, selectInput("col_pal_sankey", "配色方案", choices = c("A", "B", "C", "D", "E", "F", "G", "H"), selected = "A")),
                   column(3, numericInput("label_size_sankey", "标签大小", value = 3.1, min = 0.1, step = 0.1)),
                   column(3, colourpicker::colourInput("label_color_sankey", "标签颜色", value = "black")),
                   column(3, numericInput("node_alpha_sankey", "透明度", value = 0.7, min = 0, max = 1, step = 0.01))
                 ),

                 fluidRow(
                   column(3, numericInput("dim1_n", "维度1节点数", value = 20, min = 1, max = 40)),
                   column(3, numericInput("dim2_n", "维度2节点数", value = 5, min = 1, max = 40)),

                   # 隐藏的维度3和维度4节点数输入框
                   column(3, tags$div(id = "dim3_container", style = "display: none;",
                                      numericInput("dim3_n", "维度3节点数", value = 20, min = 1, max = 40))),
                   column(3, tags$div(id = "dim4_container", style = "display: none;",
                                      numericInput("dim4_n", "维度4节点数", value = 20, min = 1, max = 40)))
                 ),

                 withSpinner(
                   plotOutput("sankey_plot"),
                   type = 6, color = "#0275d8", size = 1
                 ),

                 hr(),

                 fluidRow(
                   column(2,
                          downloadButton("download_plot", "图片", class = "btn btn-success"),
                          style = "display: flex; align-items: flex-end; margin-bottom: 15px;"
                   ),
                   column(3,
                          selectInput("img_format", "选择格式", choices = c("pdf", "tiff"), selected = "pdf"),
                          style = "display: flex; align-items: flex-end;"
                   ),
                   column(3,
                          numericInput("img_width", "宽度（cm）", value = 8, min = 1),
                          style = "display: flex; align-items: flex-end;"
                   ),
                   column(3,
                          numericInput("img_height", "高度（cm）", value = 6, min = 1),
                          style = "display: flex; align-items: flex-end; gap: 5px;"
                   )
                 )
               )
             )
    )

  )
)
)

#### 服务器逻辑部分 ####
server <- function(input, output, session) {
  ##### 匹配字段 #####
  herb_dirty <- reactiveVal(NULL)
  herb_freq <- reactiveVal(NULL)

  observeEvent(input$file, {
    herb_dirty(read.xlsx(input$file$datapath))
  })

  observeEvent(input$clear, {
    herb_dirty(NULL)
    herb_freq(NULL)
  })

  # 字段匹配处理
  name_to_replaced <- reactive({
    req(herb_dirty())
    name_to_replaced <- makelookuptable2(herb_dirty(), type = input$type)
    colnames(name_to_replaced) <- c("待规范", "替换为", "频数")
    name_to_replaced
  })

  output$table <- renderDataTable({
    req(name_to_replaced())
    datatable(name_to_replaced(), options = list(scrollX = TRUE, scrollY = "500px", scroller = TRUE))
  })

  output$downloadData <- downloadHandler(
    filename = function() {
      paste("匹配", Sys.Date(), ".xlsx", sep="")
    },
    content = function(file) {
      write.xlsx(name_to_replaced(), file)
    }
  )

  # 频数分析处理
  observeEvent(input$file, {
    req(herb_dirty())
    herb_freq(untidy_freq(herb_dirty()))
  })

  output$freq_table <- renderDataTable({
    req(herb_freq())
    datatable(herb_freq(), options = list(scrollX = TRUE, scrollY = "500px", scroller = TRUE))
  })

  output$downloadFreq <- downloadHandler(
    filename = function() {
      paste("频数", Sys.Date(), ".xlsx", sep="")
    },
    content = function(file) {
      write.xlsx(herb_freq(), file)
    }
  )
  ##### 根据规则表清洗数据 #####
  data_dirty <- reactiveVal(NULL)
  lookuptable <- reactiveVal(NULL)
  data_clean <- reactiveVal(NULL)
  data_clean_wide <- reactiveVal(NULL)
  data_clean_long <- reactiveVal(NULL)
  current_table <- reactiveVal(NULL)

  observeEvent(input$dataFile, {
    data_dirty(read.xlsx(input$dataFile$datapath))
  })

  observeEvent(input$lookupFile, {
    lookuptable(read.xlsx(input$lookupFile$datapath))
  })

  observeEvent(input$cleanData, {
    req(data_dirty(), lookuptable())

    # 捕获警告信息
    warning_message <- NULL
    cleaned_data <- withCallingHandlers(
      clean_data(data_dirty(), lookuptable()),
      warning = function(w) {
        warning_message <<- append(warning_message, conditionMessage(w))  # 捕获警告信息
        invokeRestart("muffleWarning")  # 阻止默认的警告行为
      }
    )

    # 更新清洗数据
    data_clean(cleaned_data)
    current_table("clean")

    # 在UI中显示警告信息（如果有）
    if (!is.null(warning_message)) {
      shinyjs::show("warning-message")  # 显示警告区域
      shinyjs::html("warning-message", paste(warning_message, collapse = "<br>"))  # 显示捕获的警告信息
    } else {
      shinyjs::hide("warning-message")  # 如果没有警告，则隐藏警告区域
    }
  })

  observeEvent(input$convertToWide, {
    req(data_clean())
    wide_data <- trans_wide(data_clean())
    data_clean_wide(wide_data)
    current_table("wide")
  })

  observeEvent(input$convertToLong, {
    req(data_clean())
    long_data <- trans_long2(data_clean())
    data_clean_long(long_data)
    current_table("long")
  })

  output$resultTable <- renderDataTable({
    table_type <- current_table()
    req(table_type)
    if (table_type == "clean") {
      datatable(data_clean(), options = list(scrollX = TRUE, scrollY = "500px", scroller = TRUE))
    } else if (table_type == "wide") {
      datatable(data_clean_wide(), options = list(scrollX = TRUE, scrollY = "500px", scroller = TRUE))
    } else if (table_type == "long") {
      datatable(data_clean_long(), options = list(scrollX = TRUE, scrollY = "500px", scroller = TRUE))
    }
  })

  output$downloadCleanedData <- downloadHandler(
    filename = function() {
      paste("cleaned_data-", Sys.Date(), ".xlsx", sep="")
    },
    content = function(file) {
      write.xlsx(data_clean(), file)
    }
  )

  output$downloadWideData <- downloadHandler(
    filename = function() {
      paste("wide_data-", Sys.Date(), ".xlsx", sep="")
    },
    content = function(file) {
      write.xlsx(data_clean_wide(), file)
    }
  )

  output$downloadLongData <- downloadHandler(
    filename = function() {
      paste("long_data-", Sys.Date(), ".xlsx", sep="")
    },
    content = function(file) {
      write.xlsx(data_clean_long(), file)
    }
  )

  ##### 性味归经与功效可视化 ####
  observeEvent(input$toggle_visual_params, {
    toggle("visual_params")
  })

  observeEvent(input$toggle_efficacy_params, {
    toggle("efficacy_params")
  })

  data_clean_wide_property <- reactiveVal(NULL)
  property_result <- reactiveVal(NULL)
  efficacy_result <- reactiveVal(NULL)
  add_herb_property <- reactiveVal(NULL)  # 新增的反应值
  add_herb_efficacy <- reactiveVal(NULL)  # 新增的中药功效反应值

  observeEvent(input$propertyFile, {
    data_clean_wide_property(read.xlsx(input$propertyFile$datapath))
  })

  observeEvent(input$additionalPropertyFile, {
    add_herb_property(read.xlsx(input$additionalPropertyFile$datapath))  # 读取附加的中药属性文件
  })

  observeEvent(input$efficacyFile, {
    add_herb_efficacy(read.xlsx(input$efficacyFile$datapath))  # 读取附加的中药功效文件
  })

  observeEvent(input$calcProperty, {
    req(data_clean_wide_property())
    property <- calc_property(data_clean_wide_property(), added = add_herb_property())  # 传递新增的属性数据
    property_result(property)
    efficacy_result(calc_efficacy(data_clean_wide_property(), added = add_herb_efficacy()))  # 传递新增的功效数据
  })

  output$propertyGjTable <- renderDataTable({
    req(property_result())
    datatable(property_result()$归经, options = list(scrollX = TRUE, scrollY = "500px", scroller = TRUE))
  })

  output$propertySqTable <- renderDataTable({
    req(property_result())
    datatable(property_result()$四气, options = list(scrollX = TRUE, scrollY = "500px", scroller = TRUE))
  })

  output$propertyTasteTable <- renderDataTable({
    req(property_result())
    datatable(property_result()$五味, options = list(scrollX = TRUE, scrollY = "500px", scroller = TRUE))
  })

  output$efficacyTable <- renderDataTable({
    req(efficacy_result())
    datatable(efficacy_result(), options = list(scrollX = TRUE, scrollY = "500px", scroller = TRUE))
  })

  output$plotGjVisualization <- renderPlot({
    req(data_clean_wide_property())
    fig_guijing(data_clean_wide_property(), line_col = input$line_col, bac_col = input$bac_col, label.size = input$label_size, line.width = input$line_width, added = add_herb_property())
  })

  output$plotSqVisualization <- renderPlot({
    req(data_clean_wide_property())
    fig_siqi(data_clean_wide_property(), line_col = input$line_col, bac_col = input$bac_col, label.size = input$label_size, line.width = input$line_width, added = add_herb_property())
  })

  output$plotTasteVisualization <- renderPlot({
    req(data_clean_wide_property())
    fig_taste(data_clean_wide_property(), line_col = input$line_col, bac_col = input$bac_col, label.size = input$label_size, line.width = input$line_width, added = add_herb_property())
  })

  output$plotEfficacyVisualization <- renderPlot({
    req(data_clean_wide_property())
    fig_efficacy(data_clean_wide_property(), cal_pal = input$cal_pal, ignore = input$ignore, lab_size = input$lab_size, added = add_herb_efficacy())
  })

  output$downloadProperty <- downloadHandler(
    filename = function() {
      paste("中药属性功效-", Sys.Date(), ".xlsx", sep="")
    },
    content = function(file) {
      req(property_result())
      wb <- createWorkbook()

      # 添加中药属性结果的工作表
      addWorksheet(wb, "归经")
      writeData(wb, "归经", property_result()$归经)

      addWorksheet(wb, "四气")
      writeData(wb, "四气", property_result()$四气)

      addWorksheet(wb, "五味")
      writeData(wb, "五味", property_result()$五味)

      # 同时添加功效结果的工作表
      req(efficacy_result())
      addWorksheet(wb, "功效")
      writeData(wb, "功效", efficacy_result())

      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )


  output$downloadGjPlot <- downloadHandler(
    filename = function() {
      paste("归经-", Sys.Date(), ".", tolower(input$file_format), sep="")
    },
    content = function(file) {
      req(data_clean_wide_property())
      plot_obj <- fig_guijing(data_clean_wide_property(), line_col = input$line_col, bac_col = input$bac_col, label.size = input$label_size, line.width = input$line_width, added = add_herb_property())
      ggsave(file, plot = plot_obj, device = tolower(input$file_format),
             width = input$img_width_efficacy, height = input$img_height_efficacy, dpi = 600, units = "cm")
    }
  )

  output$downloadSqPlot <- downloadHandler(
    filename = function() {
      paste("四气-", Sys.Date(), ".", tolower(input$file_format), sep="")
    },
    content = function(file) {
      req(data_clean_wide_property())
      plot_obj <- fig_siqi(data_clean_wide_property(), line_col = input$line_col, bac_col = input$bac_col, label.size = input$label_size, line.width = input$line_width, added = add_herb_property())
      ggsave(file, plot = plot_obj, device = tolower(input$file_format),
             width = input$img_width_efficacy, height = input$img_height_efficacy, dpi = 600, units = "cm")
    }
  )

  output$downloadTastePlot <- downloadHandler(
    filename = function() {
      paste("五味-", Sys.Date(), ".", tolower(input$file_format), sep="")
    },
    content = function(file) {
      req(data_clean_wide_property())
      plot_obj <- fig_taste(data_clean_wide_property(), line_col = input$line_col, bac_col = input$bac_col, label.size = input$label_size, line.width = input$line_width, added = add_herb_property())
      ggsave(file, plot = plot_obj, device = tolower(input$file_format),
             width = input$img_width_efficacy, height = input$img_height_efficacy, dpi = 600, units = "cm")
    }
  )

  output$downloadEfficacyPlot <- downloadHandler(
    filename = function() {
      paste("功效-", Sys.Date(), ".", tolower(input$file_format), sep="")
    },
    content = function(file) {
      req(data_clean_wide_property())
      plot_obj <- fig_efficacy(data_clean_wide_property(), cal_pal = input$cal_pal, ignore = input$ignore, lab_size = input$lab_size, added = add_herb_efficacy())
      ggsave(file, plot = plot_obj, device = tolower(input$file_format),
             width = input$img_width_efficacy, height = input$img_height_efficacy, dpi = 600, units = "cm")
    }
  )

  #### 关联规则 ####
  observeEvent(input$toggle_params, {
    toggle("params")
  })

  exp_rules <- reactiveVal(NULL)
  rules_df <- reactiveVal(NULL)
  rules_val <- reactiveVal(NULL)

  observeEvent(input$exploreRules, {
    req(input$arFile)

    # 显示正在分析的标识
    shinyjs::show("analyzing")

    # 读取上传的文件
    data_clean_wide <- read.xlsx(input$arFile$datapath)

    # 转换为transactions对象
    trans <- trans_rules(data_clean_wide)

    # 获取用户输入的supp和conf值
    supp_values <- as.numeric(unlist(strsplit(input$supp_values, ",")))
    conf_values <- as.numeric(unlist(strsplit(input$conf_values, ",")))

    # 进行关联规则探索
    exp_rules_val <- explore_rules(trans, supp = supp_values, conf = conf_values)
    exp_rules(exp_rules_val)

    # 呈现规则表格
    output$rulesTable <- renderDataTable({
      datatable(exp_rules_val, options = list(scrollX = TRUE, scrollY = "500px", scroller = TRUE))
    })

    # 生成规则可视化
    output$expRulesPlot <- renderPlot({
      req(exp_rules_val)
      p1 <- fig_exp_rule(exp_rules_val, col_pal = input$col_pal)
      print(p1)
    })

    # 分析完成后隐藏标识
    shinyjs::hide("analyzing")
  })

  observeEvent(input$analyzeRules, {
    req(input$arFile)

    # 读取上传的文件
    data_clean_wide <- read.xlsx(input$arFile$datapath)

    # 获取用户输入的supp、conf和min_lift值
    supp <- input$supp
    conf <- input$conf
    min_lift <- input$min_lift

    # 进行关联规则分析
    rules <- extract_rules(data_clean_wide, supp = supp, conf = conf, min_lift = min_lift)
    rules_val(rules)

    # 将rules对象转换为data.frame对象
    rules_df_val <- as(rules, "data.frame")

    # 保留第2到第5列的3位小数
    rules_df_val[, 2:5] <- round(rules_df_val[, 2:5], digits = 3)

    rules_df(rules_df_val)

    # 呈现规则表格
    output$analyzedRulesTable <- renderDataTable({
      datatable(rules_df_val, options = list(scrollX = TRUE, scrollY = "500px", scroller = TRUE))
    })
  })

  output$rulesDistrPlot <- renderPlot({
    req(rules_val())
    p2 <- fig_rules_distr(rules_val(), col_pal = input$distr_col_pal, top = input$distr_top)
    print(p2)
  })

  observeEvent(input$shading, {
    output$rulesMatrixPlot <- renderPlot({
      req(rules_val())
      p3 <- fig_rules_bubble(rules_val(), shading = switch(input$shading, "提升度" = "lift", "置信度" = "confidence", "支持度" = "support"),
                             measure = switch(input$measure, "提升度" = "lift", "置信度" = "confidence", "支持度" = "support"),
                             col = c(input$col_min, input$col_max), alpha = input$alpha, k = input$k_rules)
      print(p3)
    })
  })

  observeEvent(input$measure, {
    output$rulesMatrixPlot <- renderPlot({
      req(rules_val())
      p3 <- fig_rules_bubble(rules_val(), shading = switch(input$shading, "提升度" = "lift", "置信度" = "confidence", "支持度" = "support"),
                             measure = switch(input$measure, "提升度" = "lift", "置信度" = "confidence", "支持度" = "support"),
                             col = c(input$col_min, input$col_max), alpha = input$alpha, k = input$k_rules)
      print(p3)
    })
  })

  observeEvent(input$col_min, {
    output$rulesMatrixPlot <- renderPlot({
      req(rules_val())
      p3 <- fig_rules_bubble(rules_val(), shading = switch(input$shading, "提升度" = "lift", "置信度" = "confidence", "支持度" = "support"),
                             measure = switch(input$measure, "提升度" = "lift", "置信度" = "confidence", "支持度" = "support"),
                             col = c(input$col_min, input$col_max), alpha = input$alpha, k = input$k_rules)
      print(p3)
    })
  })

  observeEvent(input$col_max, {
    output$rulesMatrixPlot <- renderPlot({
      req(rules_val())
      p3 <- fig_rules_bubble(rules_val(), shading = switch(input$shading, "提升度" = "lift", "置信度" = "confidence", "支持度" = "support"),
                             measure = switch(input$measure, "提升度" = "lift", "置信度" = "confidence", "支持度" = "support"),
                             col = c(input$col_min, input$col_max), alpha = input$alpha, k = input$k_rules)
      print(p3)
    })
  })

  observeEvent(input$alpha, {
    output$rulesMatrixPlot <- renderPlot({
      req(rules_val())
      p3 <- fig_rules_bubble(rules_val(), shading = switch(input$shading, "提升度" = "lift", "置信度" = "confidence", "支持度" = "support"),
                             measure = switch(input$measure, "提升度" = "lift", "置信度" = "confidence", "支持度" = "support"),
                             col = c(input$col_min, input$col_max), alpha = input$alpha, k = input$k_rules)
      print(p3)
    })
  })

  observeEvent(input$k, {
    output$rulesMatrixPlot <- renderPlot({
      req(rules_val())
      p3 <- fig_rules_bubble(rules_val(), shading = switch(input$shading, "提升度" = "lift", "置信度" = "confidence", "支持度" = "support"),
                             measure = switch(input$measure, "提升度" = "lift", "置信度" = "confidence", "支持度" = "support"),
                             col = c(input$col_min, input$col_max), alpha = input$alpha, k = input$k_rules)
      print(p3)
    })
  })

  observeEvent(input$rule_size, {
    output$rulesNetworkPlot <- renderPlot({
      req(rules_val())
      p4 <- fig_rules_network(rules_val(), rule.size = switch(input$rule_size, "支持度" = "support", "置信度" = "confidence", "提升度" = "lift"),
                              rule.color = switch(input$rule_color, "支持度" = "support", "置信度" = "confidence", "提升度" = "lift"),
                              min.val = input$min_val, max.val = input$max_val, edge_col = input$edge_col, num_rules = input$num_rules)
      print(p4)
    })
  })

  observeEvent(input$rule_color, {
    output$rulesNetworkPlot <- renderPlot({
      req(rules_val())
      p4 <- fig_rules_network(rules_val(), rule.size = switch(input$rule_size, "支持度" = "support", "置信度" = "confidence", "提升度" = "lift"),
                              rule.color = switch(input$rule_color, "支持度" = "support", "置信度" = "confidence", "提升度" = "lift"),
                              min.val = input$min_val, max.val = input$max_val, edge_col = input$edge_col, num_rules = input$num_rules)
      print(p4)
    })
  })

  observeEvent(input$min_val, {
    output$rulesNetworkPlot <- renderPlot({
      req(rules_val())
      p4 <- fig_rules_network(rules_val(), rule.size = switch(input$rule_size, "支持度" = "support", "置信度" = "confidence", "提升度" = "lift"),
                              rule.color = switch(input$rule_color, "支持度" = "support", "置信度" = "confidence", "提升度" = "lift"),
                              min.val = input$min_val, max.val = input$max_val, edge_col = input$edge_col, num_rules = input$num_rules)
      print(p4)
    })
  })

  observeEvent(input$max_val, {
    output$rulesNetworkPlot <- renderPlot({
      req(rules_val())
      p4 <- fig_rules_network(rules_val(), rule.size = switch(input$rule_size, "支持度" = "support", "置信度" = "confidence", "提升度" = "lift"),
                              rule.color = switch(input$rule_color, "支持度" = "support", "置信度" = "confidence", "提升度" = "lift"),
                              min.val = input$min_val, max.val = input$max_val, edge_col = input$edge_col, num_rules = input$num_rules)
      print(p4)
    })
  })

  observeEvent(input$edge_col, {
    output$rulesNetworkPlot <- renderPlot({
      req(rules_val())
      p4 <- fig_rules_network(rules_val(), rule.size = switch(input$rule_size, "支持度" = "support", "置信度" = "confidence", "提升度" = "lift"),
                              rule.color = switch(input$rule_color, "支持度" = "support", "置信度" = "confidence", "提升度" = "lift"),
                              min.val = input$min_val, max.val = input$max_val, edge_col = input$edge_col, num_rules = input$num_rules)
      print(p4)
    })
  })

  observeEvent(input$num_rules, {
    output$rulesNetworkPlot <- renderPlot({
      req(rules_val())
      p4 <- fig_rules_network(rules_val(), rule.size = switch(input$rule_size, "支持度" = "support", "置信度" = "confidence", "提升度" = "lift"),
                              rule.color = switch(input$rule_color, "支持度" = "support", "置信度" = "confidence", "提升度" = "lift"),
                              min.val = input$min_val, max.val = input$max_val, edge_col = input$edge_col, num_rules = input$num_rules)
      print(p4)
    })
  })

  output$downloadExpRules <- downloadHandler(
    filename = function() {
      paste("探索规则表-", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      write.xlsx(exp_rules(), file)
    }
  )

  output$downloadRules <- downloadHandler(
    filename = function() {
      paste("关联规则表-", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      write.xlsx(rules_df(), file)
    }
  )

  output$downloadExpPlot <- downloadHandler(
    filename = function() {
      paste("探索规则可视化-", Sys.Date(), ".", tolower(input$expPlotType), sep = "")
    },
    content = function(file) {
      ggsave(file, plot = fig_exp_rule(exp_rules(), col_pal = input$col_pal),
             device = tolower(input$expPlotType), width = input$expPlotWidth, height = input$expPlotHeight, dpi = 600, units = "cm")
    }
  )

  output$downloadDistrPlot <- downloadHandler(
    filename = function() {
      paste("规则分布可视化-", Sys.Date(), ".", tolower(input$distrPlotType), sep = "")
    },
    content = function(file) {
      ggsave(file, plot = fig_rules_distr(rules_val(), col_pal = input$distr_col_pal, top = input$distr_top),
             device = tolower(input$distrPlotType), width = input$distrPlotWidth, height = input$distrPlotHeight, dpi = 600, units = "cm")
    }
  )

  output$downloadMatrixPlot <- downloadHandler(
    filename = function() {
      paste("规则矩阵可视化-", Sys.Date(), ".", tolower(input$matrixPlotType), sep = "")
    },
    content = function(file) {
      ggsave(file, plot = fig_rules_bubble(rules_val(), shading = switch(input$shading, "提升度" = "lift", "置信度" = "confidence", "支持度" = "support"),
                                           measure = switch(input$measure, "提升度" = "lift", "置信度" = "confidence", "支持度" = "support"),
                                           col = c(input$col_min, input$col_max), alpha = input$alpha, k = input$k_rules),
             device = tolower(input$matrixPlotType), width = input$matrixPlotWidth, height = input$matrixPlotHeight, dpi = 600, units = "cm")
    }
  )

  output$downloadNetworkPlot <- downloadHandler(
    filename = function() {
      paste("规则网络可视化-", Sys.Date(), ".", tolower(input$networkPlotType), sep = "")
    },
    content = function(file) {
      ggsave(file, plot = fig_rules_network(rules_val(), rule.size = switch(input$rule_size, "支持度" = "support", "置信度" = "confidence", "提升度" = "lift"),
                                            rule.color = switch(input$rule_color, "支持度" = "support", "置信度" = "confidence", "提升度" = "lift"),
                                            min.val = input$min_val, max.val = input$max_val, edge_col = input$edge_col, num_rules = input$num_rules),
             device = tolower(input$networkPlotType), width = input$networkPlotWidth, height = input$networkPlotHeight, dpi = 600, units = "cm")
    }
  )

  ##### 处方相似度 #####
  data_clean_wide <- reactiveVal(NULL)
  sim_res <- reactiveVal(NULL)
  sim_heatmap <- reactiveVal(NULL)

  observeEvent(input$simFile, {
    req(input$simFile)
    data_clean_wide(read.xlsx(input$simFile$datapath))
  })

  observeEvent(input$analyzeSim, {
    req(data_clean_wide())
    res <- calc_jaccard(data_clean_wide(), index = input$index)
    sim_res(res)

    output$simMatrixTable <- renderDataTable({
      req(sim_res())
      datatable(sim_res()$jaccard_screen, options = list(scrollX = TRUE, scrollY = "500px", scroller = TRUE))
    })

    output$simHeatmap <- renderPlot({
      req(sim_res())
      jac_p <- fig_jac_shiny(sim_res(), col_pal = input$jaccard_colpal, cell_size = input$cell_size)
      sim_heatmap(jac_p)
      print(jac_p)
    })
  })

  output$downloadSimData <- downloadHandler(
    filename = function() {
      paste("处方相似度数据-", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      write.xlsx(sim_res()$jaccard_screen, file)
    }
  )

  output$downloadHeatmap <- downloadHandler(
    filename = function() {
      paste("处方相似度热图-", Sys.Date(), ".", tolower(input$heatmapType), sep = "")
    },
    content = function(file) {
      ggsave(file, plot = sim_heatmap(), device = tolower(input$heatmapType),
             width = input$heatmapWidth, height = input$heatmapHeight, dpi = 600, units = "cm")
    }
  )


  ##### Phi#####
  data_clean_wide <- reactiveVal(NULL)
  phi_res <- reactiveVal(NULL)

  observeEvent(input$clusterFile, {
    req(input$clusterFile)
    data_clean_wide(read.xlsx(input$clusterFile$datapath))
  })

  observeEvent(input$analyzePhi, {
    req(data_clean_wide())
    res <- calc_Phi(data_clean_wide(), top = input$top_phi)
    phi_res(res)

    output$phiMatrixTable <- renderDataTable({
      req(phi_res())
      datatable(phi_res()$phi_matrix, options = list(scrollX = TRUE, scrollY = "500px", scroller = TRUE))
    })

    output$phiClusterPlot <- renderPlot({
      req(phi_res())
      phi_p <- fig_phi_shiny(
        phi_res(),
        col_pal = input$phi_cluster_colpal,
        k = input$k_phi,
        fontsize = input$fontsize,
        border_color = input$border_color,
        cell_cize = input$cell_cize,
        font_angle = input$font_angle,
        tree_height = input$tree_height
      )
      print(phi_p)
    })
  })

  output$downloadPhiPlot <- downloadHandler(
    filename = function() {
      paste("PhiClusterPlot-", Sys.Date(), ".", tolower(input$phiPlotType), sep = "")
    },
    content = function(file) {
      ggsave(file, plot = fig_phi_shiny(
        phi_res(),
        col_pal = input$phi_cluster_colpal,
        k = input$k_phi,
        fontsize = input$fontsize,
        border_color = input$border_color,
        cell_cize = input$cell_cize,
        font_angle = input$font_angle,
        tree_height = input$tree_height
      ), device = tolower(input$phiPlotType),
      width = input$phiPlotWidth, height = input$phiPlotHeight, dpi = 600, units = "cm")
    }
  )
  ##### 共现网络 ####
  data_cooc <- reactiveVal(NULL)
  cooc_net <- reactiveVal(NULL)

  observeEvent(input$coocFile, {
    req(input$coocFile)
    data_cooc(read.xlsx(input$coocFile$datapath))
  })

  observeEvent(input$generateCooc, {
    req(data_cooc())
    net <- cooc_graph(data_cooc(), top = input$top_n, min = input$min_threshold)
    cooc_net(net)

    output$nodesTable <- renderDataTable({
      req(cooc_net())
      datatable(cooc_net()$nodes, options = list(scrollX = TRUE, scrollY = "500px", scroller = TRUE))
    })

    output$edgesTable <- renderDataTable({
      req(cooc_net())
      datatable(cooc_net()$edges, options = list(scrollX = TRUE, scrollY = "500px", scroller = TRUE))
    })

    output$coocPlot <- renderPlot({
      req(cooc_net())
      cooc_p <- fig_cooc(
        cooc_net(),
        col_pal = input$col_pal_cooc,
        line_type = input$line_type,
        font_color = input$font_color,
        edge_col_low = input$edge_col_low,
        edge_col_high = input$edge_col_high,
        node_layout = input$node_layout,
        node_alpha = input$node_alpha,
        text_size = input$text_size_cooc,
        edge_width = input$edge_width,
        node_size = input$node_size
      )
      print(cooc_p)
    })
  })

  # 下载共现网络数据为Excel
  output$downloadCoocNet <- downloadHandler(
    filename = function() {
      paste("cooc_net-", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      req(cooc_net())
      write.xlsx(cooc_net(), file)
    }
  )

  # 下载共现网络图片
  output$downloadCoocPlot <- downloadHandler(
    filename = function() {
      paste("cooc_plot-", Sys.Date(), ".", tolower(input$plotFormat), sep = "")
    },
    content = function(file) {
      req(cooc_net())
      cooc_p <- fig_cooc(
        cooc_net(),
        col_pal = input$col_pal_cooc,
        line_type = input$line_type,
        font_color = input$font_color,
        edge_col_low = input$edge_col_low,
        edge_col_high = input$edge_col_high,
        node_layout = input$node_layout,
        node_alpha = input$node_alpha,
        text_size = input$text_size_cooc,
        edge_width = input$edge_width,
        node_size = input$node_size
      )
      ggsave(file, plot = cooc_p, device = tolower(input$plotFormat),
             width = input$plotWidth, height = input$plotHeight, dpi = 600, units = "cm")
    }
  )

  #### 剂量分析 ####

  # 反应式数据载入
  data_dose <- reactive({
    req(input$file1)
    inFile <- input$file1
    read.xlsx(inFile$datapath)
  })

  # 在点击“分析”按钮后才执行分析
  dose_distr <- eventReactive(input$analyze, {
    req(data_dose())
    calc_herb_dosage(data_dose())
  })

  # 使用DT展示表格，带有滚动条和分页功能
  output$dose_table <- renderDT({
    req(dose_distr())
    datatable(dose_distr(),
              options = list(scrollX = TRUE, pageLength = 10))
  })

  # 生成可视化图表
  dose_p <- eventReactive(input$analyze, {
    req(data_dose())
    fig_dose(data_dose(),
             top = input$top_dose,
             percent_ingore = input$percent_ingore,
             bar_line_size = input$bar_line_size,
             bar_alpha = input$bar_alpha,
             text_size = input$text_size_dose,
             bar_pal = input$dose_colpal,
             min_col = input$min_col,
             max_col = input$max_col,
             mean_col = input$mean_col,
             dumbbell_col = input$dumbbell_col)
  })

  output$dose_plot <- renderPlot({
    req(dose_p())
    dose_p()
  })

  # 下载表格为Excel文件
  output$download_table <- downloadHandler(
    filename = function() {
      paste("dose_table", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      write.xlsx(dose_distr(), file)
    }
  )

  # 图片下载功能，确保图片为600 DPI
  output$download_plot_dose <- downloadHandler(
    filename = function() {
      paste("dose_plot", Sys.Date(), ".", input$img_format_dose, sep = "")
    },
    content = function(file) {
      ggsave(file, plot = dose_p(),
             device = tolower(input$img_format),
             width = input$img_width_dose,
             height = input$img_height_dose,
             units = "cm",
             dpi = 600)
    }
  )


  #### 综合分析 ####
  # 使用 reactiveValues 管理数据
  data_values <- reactiveValues(
    dim1 = NULL,
    dim2 = NULL,
    dim3 = NULL,
    dim4 = NULL
  )

  # 用于存储绘图对象
  plot_object <- reactiveVal(NULL)

  # 动态UI显示文件上传控件和删除按钮
  output$file3_ui <- renderUI({
    tagList(
      fluidRow(
        column(9, fileInput("file3", "维度3", accept = c(".xlsx"), placeholder = ".xlsx文件")),
        column(3, actionButton("remove_file3", "删", class = "btn btn-danger",
                               style = "margin-top: 32px; display: flex; justify-content: center;"))
      )
    )
  })

  output$file4_ui <- renderUI({
    tagList(
      fluidRow(
        column(9, fileInput("file4", "维度4", accept = c(".xlsx"), placeholder = ".xlsx文件")),
        column(3, actionButton("remove_file4", "删", class = "btn btn-danger",
                               style = "margin-top: 32px; display: flex; justify-content: center;"))
      )
    )
  })

  # 文件上传逻辑
  observe({
    if (!is.null(input$file1_sankey)) {
      tryCatch({
        data_values$dim1 <- read.xlsx(input$file1_sankey$datapath)
      }, error = function(e) {
        showNotification("维度1文件读取失败，请检查文件格式！", type = "error")
      })
    }
  })

  observe({
    if (!is.null(input$file2_sankey)) {
      tryCatch({
        data_values$dim2 <- read.xlsx(input$file2_sankey$datapath)
      }, error = function(e) {
        showNotification("维度2文件读取失败，请检查文件格式！", type = "error")
      })
    }
  })

  observe({
    if (!is.null(input$file3)) {
      tryCatch({
        data_values$dim3 <- read.xlsx(input$file3$datapath)
      }, error = function(e) {
        showNotification("维度3文件读取失败，请检查文件格式！", type = "error")
      })
    }
  })

  observe({
    if (!is.null(input$file4)) {
      tryCatch({
        data_values$dim4 <- read.xlsx(input$file4$datapath)
      }, error = function(e) {
        showNotification("维度4文件读取失败，请检查文件格式！", type = "error")
      })
    }
  })

  # 删除文件逻辑
  observeEvent(input$remove_file3, {
    shinyjs::reset("file3")  # 使用shinyjs重置文件输入
    data_values$dim3 <- NULL
    shinyjs::hide("dim3_container")  # 隐藏维度3节点数输入框
  })

  observeEvent(input$remove_file4, {
    shinyjs::reset("file4")  # 使用shinyjs重置文件输入
    data_values$dim4 <- NULL
    shinyjs::hide("dim4_container")  # 隐藏维度4节点数输入框
  })

  # 动态显示/隐藏维度3和维度4节点数输入框
  observeEvent(input$file3, {
    shinyjs::toggle(id = "dim3_container", condition = !is.null(input$file3))
  })

  observeEvent(input$file4, {
    shinyjs::toggle(id = "dim4_container", condition = !is.null(input$file4))
  })

  # 绘制桑基图
  output$sankey_plot <- renderPlot({
    req(input$generate_sankey)
    req(data_values$dim1, data_values$dim2)

    dim1 <- data_values$dim1
    dim2 <- data_values$dim2
    dim3 <- data_values$dim3
    dim4 <- data_values$dim4

    # 调用绘图函数并保存绘图对象
    p <- fig_sankey_shiny(
      dim1 = dim1, dim2 = dim2, dim3 = dim3, dim4 = dim4,
      dim1_n = input$dim1_n, dim2_n = input$dim2_n,
      dim3_n = input$dim3_n, dim4_n = input$dim4_n,
      col_pal = input$col_pal_sankey, label_size = input$label_size_sankey,
      label_color = input$label_color_sankey, node_alpha = input$node_alpha_sankey
    )

    # 将绘图对象存储到 reactiveVal 中
    plot_object(p)

    # 返回绘图对象以显示在页面上
    p
  })

  # 下载图片
  output$download_plot <- downloadHandler(
    filename = function() {
      paste("sankey_plot", Sys.Date(), ".", input$img_format, sep = "")
    },
    content = function(file) {
      # 获取存储的绘图对象
      p <- plot_object()
      if (is.null(p)) {
        stop("没有可用的绘图对象，请先生成图表！")
      }

      # 保存图像
      ggsave(
        file, plot = p, device = tolower(input$img_format),
        width = input$img_width, height = input$img_height,
        units = "cm", dpi = 600
      )
    }
  )
}

# 运行应用
shinyApp(ui = ui, server = server)

