source("global.R", local = TRUE)

shinyServer(function(input, output, session) {
  toc_entries <- c("TAB1", "TAB2", "TAB3", "more")
  toc_entriesID <- c("Binomial", "EndpointTBD1", "EndpointTBD2", "About")
  observe({
    local({
      lapply(1:4, function(x) {
        id <- paste0("toc_", tolower(toc_entries[x]))
        shinyjs::onclick(id, updateTabsetPanel(session, "nav", selected = toc_entriesID[x]))
      })
    })
  })


  observe({
    # Load inputs
    INFILE <- input$inputfile
    if (is.null(INFILE)) {
      return(NULL)
    }

    uploaded_inputs <- read.csv(INFILE$datapath)
    # Update each input
    for (i in 1:nrow(uploaded_inputs)) {
      updateNumericInput(session,
        inputId = uploaded_inputs$inputId[i],
        value = uploaded_inputs$value[i]
      )
    }
  })

  #         observeEvent(input$save_inputs, {
  #           # Define inputs to save
  #           inputs_to_save <<- c('alpha_con1','beta_con1','alpha_trial1','beta_trial1',  #Prior tab inputs
  #                                'post_n','postlook1','cut_B','cut_W',  #Design tab inputs -- posterior
  #                                'pred_n','predlook1','cut_B_pred','cut_W_pred','pred_tU','pred_tL',  #Design tab inputs -- predictive
  #                                'posttrue_p1','posttrue_p2','posttrue_p3','post_tU','post_tL','nsim','postOCseed', #OC evaluate tab inputs
  #                                'resp' #Analysis tab inputs
  #                                )
  #           # Declare inputs
  #           inputs <<- NULL
  #           # Append all inputs before saving to folder
  #           for(input.i in inputs_to_save){
  #             inputs <<- append(inputs, input[[input.i]])
  #           }
  #           # Inputs data.frame
  #           inputs_data_frame <<- data.frame(inputId = inputs_to_save, value = inputs)
  #           # Save Inputs
  #           save.dir = tk_choose.dir(getwd(), "Choose a location to save to:")
  #
  #           write.csv(inputs_data_frame, file = paste(save.dir,"/"
  #                                                     # ,Sys.time()
  #                                                     ,"_user_inputs.csv",sep=""), row.names = FALSE)
  #         })
  #

  output$save_inputs <- downloadHandler(
    filename = function() {
      paste("user_inputs-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      # Define inputs to save
      inputs_to_save <- c(
        "designtype",
        "alpha_con1", "beta_con1", "alpha_trial1", "beta_trial1", # Prior tab inputs
        "post_n", "postlook1", "cut_B", "cut_W", # Design tab inputs -- posterior
        "pred_n", "predlook1", "cut_B_pred", "cut_W_pred", "pred_tU", "pred_tL", # Design tab inputs -- predictive
        "posttrue_p1", "posttrue_p2", "posttrue_p3", "post_tU", "post_tL", "nsim", "postOCseed", # OC evaluate tab inputs
        "predtrue_p1", "predtrue_p2", "predtrue_p3", "pred_phiFu", "pred_phiU", "prednsim", "predOCseed", # OC pred;
        "resp" # Analysis tab inputs;
      )
      # Declare inputs
      inputs <- NULL
      # Append all inputs before saving to folder
      for (input.i in inputs_to_save) {
        inputs <- append(inputs, input[[input.i]])
      }
      # Inputs data.frame
      inputs_data_frame <- data.frame(inputId = inputs_to_save, value = inputs)
      # Save Inputs
      #                                   save.dir = tk_choose.dir(getwd(), "Choose a location to save to:")
      #
      #                                   write.csv(inputs_data_frame, file = paste(save.dir,"/"
      # ,Sys.time()

      write.csv(inputs_data_frame, file)
    }
  )

  source("Core1_priorplots.R", local = TRUE)

  source("Core2_design_prob.R", local = TRUE)
  source("Core3_design_prob_plot.R", local = TRUE)
  source("Core4_design_pred.R", local = TRUE)
  source("Core5_design_pred_plot.R", local = TRUE)

  source("Core6_OC_post.R", local = TRUE)
  source("Core7_OC_pred.R", local = TRUE)

  source("Core8_analysis_prob.R", local = TRUE)
  source("Core9_analysis_pred.R", local = TRUE)
})
