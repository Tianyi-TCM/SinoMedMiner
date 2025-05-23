library(shiny)
library(shinyjs)
library(openxlsx)
library(stringr)
library(shinyWidgets)
library(DT)
library(SinoMedminer)

# 创建用户界面 (UI)
ui <- fluidPage(
  
  useShinyjs(),
  
  # 自定义CSS样式
  tags$head(tags$style(HTML("
    body {
      font-family: 'Arial', sans-serif;
    }
    .title-panel {
      background: linear-gradient(to right, rgba(0, 70, 127, 0.2), rgba(0, 70, 127, 1), rgba(165, 204, 130, 1), rgba(165, 204, 130, 0.2)); 
      color: white;
      padding: 10px;  /* 增加高度 */
      text-align: center;
      border-radius: 5px;
      margin-bottom: 20px; /* 增加标题与内容之间的间距 */
      box-shadow: 0px 4px 8px rgba(0, 0, 0, 0.1); /* 添加阴影 */
    }
    .btn-custom {
      background-color: rgba(70, 130, 180, 0.8);  /* 蓝灰色，带透明度 */
      color: white;
      border: none;
      padding: 10px 20px;
      border-radius: 5px;
      cursor: pointer;
      transition: background-color 0.3s; /* 添加过渡效果 */
    }
    .btn-custom:hover {
      background-color: rgba(60, 120, 170, 0.8);  /* 深蓝灰色，带透明度 */
    }
    .input-group input, .input-group button {
      height: 38px;
    }
    .form-inline {
      display: flex;
      align-items: center;
    }
    .form-inline .form-group {
      margin-right: 10px; /* 增加间距 */
    }
    .form-inline input {
      width: 200px; /* 增加输入框宽度 */
    }
    .button-group {
      display: flex;
      align-items: center;
      gap: 10px;
    }
    .suggestion-buttons {
      display: flex;
      flex-wrap: wrap;
      gap: 5px;
      margin-top: 5px;
    }
    .suggestion-buttons button {
      margin-right: 5px;
    }
    .form-inline .form-group label {
      white-space: nowrap;
    }
    .text-area-custom {
      height: 60px; /* 增加高度 */
    }
    .table-container {
      box-shadow: 0px 4px 8px rgba(0, 0, 0, 0.1); /* 添加阴影 */
      border-radius: 5px;
      overflow: hidden;
    }
    hr {
      border: 1px solid rgba(169, 169, 169, 0.5); /* 调整分隔线颜色和透明度 */
      box-shadow: 0 0 10px rgba(169, 169, 169, 0.5); /* 添加阴影效果 */
    }
  "))),
  
  # JavaScript代码监听快捷键和选择事件，并禁用自动补全
  tags$script(HTML("
    $(document).on('keydown', function(e) {
      if (e.ctrlKey && e.key === 'Enter') {
        $('#submit').click();
      }
    });

    Shiny.addCustomMessageHandler('focusNextInput', function(message) {
      $('#' + message.id).focus();
    });

    $(document).on('shiny:sessioninitialized', function() {
      $('#id').attr('autocomplete', 'off');
      $('#diagnosis').attr('autocomplete', 'off');
      $('#zhenghou').attr('autocomplete', 'off');
      $('#symptom').attr('autocomplete', 'off');
      $('#prescription').attr('autocomplete', 'off');
      $('#tongue_pulse').attr('autocomplete', 'off');
      $('#clear_row').attr('autocomplete', 'off');
    });
  ")),
  
  div(class = "title-panel", 
      titlePanel("SinoMedminer 数据录入")
  ),
  
  # 录入部分
  fluidRow(
    column(3, textInput("id", "ID:", placeholder = "请输入ID")),
    column(2, textInput("diagnosis", "诊断:", placeholder = "请输入诊断")),
    column(2, uiOutput("diagnosis_suggestions")),
    column(2, textInput("zhenghou", "证候:", placeholder = "请输入证候")),
    column(2, uiOutput("zhenghou_suggestions"))
  ),
  fluidRow(
    column(3, tags$div(class = "form-group shiny-input-container", 
                       tags$label("症状:", `for` = "symptom"),
                       tags$textarea(id = "symptom", class = "form-control text-area-custom", placeholder = "请输入症状")
    )),
    column(3, tags$div(class = "form-group shiny-input-container", 
                       tags$label("处方:", `for` = "prescription"),
                       tags$textarea(id = "prescription", class = "form-control text-area-custom", placeholder = "请输入处方")
    )),
    column(2, offset = 1, textInput("tongue_pulse", "舌脉:", placeholder = "请输入舌脉"))  # 调整舌脉的位置，使其与证候对齐
  ),
  fluidRow(
    column(3,
           div(class = "form-inline",
               div(class = "form-group",
                   tags$label("清除行号:", `for` = "clear_row")
               ),
               div(class = "form-group",
                   textInput("clear_row", NULL, placeholder = "请输入要清除的行号")
               )
           )
    )
  ),
  fluidRow(
    column(3, div(actionButton("clear_data", "清除行数据", class = "btn-custom", style = "margin-top: 10px;"))),
    column(3, div(actionButton("submit", "提交", class = "btn-custom", style = "margin-top: 10px;"))),
    column(3, div(downloadButton("download_data", "下载数据", class = "btn-custom", style = "margin-top: 10px;")))
  ),
  
  # 分隔元素，增加带有荧光效果的暗灰色
  tags$hr(),
  
  # 表格和按钮部分
  div(class = "table-container",
      fluidRow(
        column(12, 
               dataTableOutput("table")
        )
      )
  )
)

# 创建服务器逻辑 (server)
server <- function(input, output, session) {
  
  input_values <- reactiveValues(data = data.frame(id = character(), diagnosis = character(), zhenghou = character(), symptom = character(), prescription = character(), tongue_pulse = character(), stringsAsFactors = FALSE))
  
  observeEvent(input$diagnosis, {
    if (!is.null(input$diagnosis) && input$diagnosis != "") {
      # 检查是否存在匹配的诊断
      matching_diagnoses <- unique(input_values$data$diagnosis[str_detect(input_values$data$diagnosis, fixed(input$diagnosis, ignore_case = TRUE))])
      output$diagnosis_suggestions <- renderUI({
        if (length(matching_diagnoses) > 0) {
          pickerInput(
            inputId = "diagnosis_picker",
            label = "选择诊断:",
            choices = matching_diagnoses,
            multiple = FALSE,
            options = pickerOptions(
              liveSearch = TRUE,
              liveSearchPlaceholder = "选择一个诊断...",
              size = 5
            )
          )
        } else {
          NULL
        }
      })
    } else {
      output$diagnosis_suggestions <- renderUI(NULL)
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$zhenghou, {
    if (!is.null(input$zhenghou) && input$zhenghou != "") {
      # 检查是否存在匹配的证候
      matching_zhenghous <- unique(input_values$data$zhenghou[str_detect(input_values$data$zhenghou, fixed(input$zhenghou, ignore_case = TRUE))])
      output$zhenghou_suggestions <- renderUI({
        if (length(matching_zhenghous) > 0) {
          pickerInput(
            inputId = "zhenghou_picker",
            label = "选择证候:",
            choices = matching_zhenghous,
            multiple = FALSE,
            options = pickerOptions(
              liveSearch = TRUE,
              liveSearchPlaceholder = "选择一个证候...",
              size = 5
            )
          )
        } else {
          NULL
        }
      })
    } else {
      output$zhenghou_suggestions <- renderUI(NULL)
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$diagnosis_picker, {
    updateTextInput(session, "diagnosis", value = input$diagnosis_picker)
    output$diagnosis_suggestions <- renderUI(NULL)
    session$sendCustomMessage("focusNextInput", list(id = "zhenghou"))
  })
  
  observeEvent(input$zhenghou_picker, {
    updateTextInput(session, "zhenghou", value = input$zhenghou_picker)
    output$zhenghou_suggestions <- renderUI(NULL)
  })
  
  observeEvent(input$submit, {
    input_values$data <- rbind(input_values$data, data.frame(
      id = input$id,
      diagnosis = input$diagnosis,
      zhenghou = input$zhenghou,
      symptom = input$symptom,
      prescription = input$prescription,
      tongue_pulse = input$tongue_pulse
    ))
    
    updateTextInput(session, "id", value = "")
    updateTextInput(session, "diagnosis", value = "")
    updateTextInput(session, "zhenghou", value = "")
    updateTextAreaInput(session, "symptom", value = "")
    updateTextAreaInput(session, "prescription", value = "")
    updateTextInput(session, "tongue_pulse", value = "")
  })
  
  observeEvent(input$clear_data, {
    clear_row <- as.numeric(input$clear_row)
    if (is.null(clear_row) || is.na(clear_row) || clear_row <= 0 || clear_row > nrow(input_values$data)) {
      showModal(modalDialog(
        title = "警告",
        "请输入有效的行号。",
        easyClose = TRUE,
        footer = NULL
      ))
    } else {
      # 删除指定行号的数据行
      input_values$data <- input_values$data[-clear_row, ]
      
      # 清空清除行号输入框
      updateTextInput(session, "clear_row", value = "")
    }
  })
  
  output$table <- renderDataTable({
    # 修改输出表格的列名
    data <- input_values$data
    colnames(data) <- c("ID", "诊断", "证候", "症状", "处方", "舌脉")
    datatable(data, options = list(
      pageLength = 10,
      scrollY = "300px",
      scrollCollapse = TRUE,
      paging = FALSE,
      dom = 't',
      fixedHeader = TRUE
    ))
  })
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("处方数据_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      if (nrow(input_values$data) == 0) {
        showModal(modalDialog(
          title = "警告",
          "数据为空，无法下载。",
          easyClose = TRUE,
          footer = NULL
        ))
        return()
      }
      
      wb <- createWorkbook()
      addWorksheet(wb, "处方数据")
      writeData(wb, sheet = 1, x = input_values$data)
      
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
}

# 运行Shiny应用
shinyApp(ui = ui, server = server)
