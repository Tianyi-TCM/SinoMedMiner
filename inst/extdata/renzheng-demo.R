library(shiny)
library(shinymanager)

credentials <- data.frame(
  user = c("test", "yuchuan"),
  password = c("yuchuan", "950201"),
  # password will automatically be hashed
  admin = c(FALSE, TRUE),
  stringsAsFactors = FALSE
)

library(keyring)
key_set("shinymanager-key", "yuchuan") # 12345

create_db(
  credentials_data = credentials,
  sqlite_path = "/srv/shiny-server/database.sqlite", # will be created
  # passphrase = key_get("shinymanager-key", "yuchuan")
  passphrase = "yuchuan0201"
)

# ui请复制app.R的

ui <- secure_app(ui, enable_admin = TRUE)


server <- function(input, output, session) {

  # check_credentials directly on sqlite db
  res_auth <- secure_server(
    check_credentials = check_credentials(
      "inst/shiny/database.sqlite",
      #passphrase = key_get("shinymanager-key", "yuchuan")
      passphrase = "yuchuan0201"
    )
  )

  output$auth_output <- renderPrint({
    reactiveValuesToList(res_auth)
  })

  # your classic server logic
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

  observeEvent(input$propertyFile, {
    data_clean_wide_property(read.xlsx(input$propertyFile$datapath))
  })

  observeEvent(input$calcProperty, {
    req(data_clean_wide_property())
    property <- calc_property(data_clean_wide_property())

    # 修改列名
    colnames(property$siqi) <- c("四气", "加权频数", "频数")
    colnames(property$meridian) <- c("归经", "加权频数", "频数")
    colnames(property$taste) <- c("五味", "加权频数", "频数")

    property_result(property)
    efficacy_result(calc_efficacy(data_clean_wide_property()))
  })

  output$propertyGjTable <- renderDataTable({
    req(property_result())
    datatable(property_result()$meridian, options = list(scrollX = TRUE, scrollY = "500px", scroller = TRUE))
  })

  output$propertySqTable <- renderDataTable({
    req(property_result())
    datatable(property_result()$siqi, options = list(scrollX = TRUE, scrollY = "500px", scroller = TRUE))
  })

  output$propertyTasteTable <- renderDataTable({
    req(property_result())
    datatable(property_result()$taste, options = list(scrollX = TRUE, scrollY = "500px", scroller = TRUE))
  })

  output$efficacyTable <- renderDataTable({
    req(efficacy_result())
    datatable(efficacy_result(), options = list(scrollX = TRUE, scrollY = "500px", scroller = TRUE))
  })

  output$plotGjVisualization <- renderPlot({
    req(data_clean_wide_property())
    fig_guijing(data_clean_wide_property(), line_col = input$line_col, bac_col = input$bac_col, label.size = input$label_size, line.width = input$line_width)
  })

  output$plotSqVisualization <- renderPlot({
    req(data_clean_wide_property())
    fig_siqi(data_clean_wide_property(), line_col = input$line_col, bac_col = input$bac_col, label.size = input$label_size, line.width = input$line_width)
  })

  output$plotTasteVisualization <- renderPlot({
    req(data_clean_wide_property())
    fig_taste(data_clean_wide_property(), line_col = input$line_col, bac_col = input$bac_col, label.size = input$label_size, line.width = input$line_width)
  })

  output$plotEfficacyVisualization <- renderPlot({
    req(data_clean_wide_property())
    fig_efficacy(data_clean_wide_property(), cal_pal = input$cal_pal, ignore = input$ignore, lab_size = input$lab_size)
  })

  observeEvent(input$visualizeGj, {
    output$plotGjVisualization <- renderPlot({
      req(data_clean_wide_property())
      fig_guijing(data_clean_wide_property(), line_col = input$line_col, bac_col = input$bac_col, label.size = input$label_size, line.width = input$line_width)
    })
  })

  observeEvent(input$visualizeSq, {
    output$plotSqVisualization <- renderPlot({
      req(data_clean_wide_property())
      fig_siqi(data_clean_wide_property(), line_col = input$line_col, bac_col = input$bac_col, label.size = input$label_size, line.width = input$line_width)
    })
  })

  observeEvent(input$visualizeTaste, {
    output$plotTasteVisualization <- renderPlot({
      req(data_clean_wide_property())
      fig_taste(data_clean_wide_property(), line_col = input$line_col, bac_col = input$bac_col, label.size = input$label_size, line.width = input$line_width)
    })
  })

  observeEvent(input$visualizeEfficacy, {
    output$plotEfficacyVisualization <- renderPlot({
      req(data_clean_wide_property())
      fig_efficacy(data_clean_wide_property(), cal_pal = input$cal_pal, ignore = input$ignore, lab_size = input$lab_size)
    })
  })

  output$downloadProperty <- downloadHandler(
    filename = function() {
      paste("property_result-", Sys.Date(), ".xlsx", sep="")
    },
    content = function(file) {
      req(property_result())
      wb <- createWorkbook()

      addWorksheet(wb, "归经")
      writeData(wb, "归经", property_result()$meridian)

      addWorksheet(wb, "四气")
      writeData(wb, "四气", property_result()$siqi)

      addWorksheet(wb, "五味")
      writeData(wb, "五味", property_result()$taste)

      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )

  output$downloadGjPlot <- downloadHandler(
    filename = function() {
      paste("guijing-", Sys.Date(), ".", tolower(input$file_format), sep="")
    },
    content = function(file) {
      ggsave(file, plot = fig_guijing(data_clean_wide_property(), line_col = input$line_col, bac_col = input$bac_col, label.size = input$label_size, line.width = input$line_width),
             device = tolower(input$file_format), width = input$img_width_property, height = input$img_height_property, dpi = 600, units = "cm")
    }
  )

  output$downloadSqPlot <- downloadHandler(
    filename = function() {
      paste("siqi-", Sys.Date(), ".", tolower(input$file_format), sep="")
    },
    content = function(file) {
      ggsave(file, plot = fig_siqi(data_clean_wide_property(), line_col = input$line_col, bac_col = input$bac_col, label.size = input$label_size, line.width = input$line_width),
             device = tolower(input$file_format), width = input$img_width_property, height = input$img_height_property, dpi = 600, units = "cm")
    }
  )

  output$downloadTastePlot <- downloadHandler(
    filename = function() {
      paste("taste-", Sys.Date(), ".", tolower(input$file_format), sep="")
    },
    content = function(file) {
      ggsave(file, plot = fig_taste(data_clean_wide_property(), line_col = input$line_col, bac_col = input$bac_col, label.size = input$label_size, line.width = input$line_width),
             device = tolower(input$file_format), width = input$img_width_property, height = input$img_height_property, dpi = 600, units = "cm")
    }
  )

  output$downloadEfficacyPlot <- downloadHandler(
    filename = function() {
      paste("efficacy-", Sys.Date(), ".", tolower(input$file_format), sep="")
    },
    content = function(file) {
      ggsave(file, plot = fig_efficacy(data_clean_wide_property(), cal_pal = input$cal_pal, ignore = input$ignore, lab_size = input$lab_size),
             device = tolower(input$file_format), width = input$img_width_efficacy, height = input$img_height_efficacy, dpi = 600, units = "cm")
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
  output$download_plot <- downloadHandler(
    filename = function() {
      paste("dose_plot", Sys.Date(), ".", input$img_format, sep = "")
    },
    content = function(file) {
      ggsave(file, plot = dose_p(),
             device = tolower(input$img_format),
             width = input$img_width_dose,
             height = input$img_height,
             units = "cm",
             dpi = 600)
    }
  )
}

shinyApp(ui = ui, server = server)


