# UI-elements for DOE.R

# CRD Analysis ------------------------------------------------------------

# variable selection - CRD Analysis
output$crd_depvar <- reactiveUI(function() {
  vars <- varnames()
  if(is.null(vars)) return()
  isFct <- sapply(getdata(), is.numeric)
  vars <- vars[isFct]
  selectInput(inputId = "crd_depvar", label = "Select response variable:", choices = vars, selected = NULL, multiple = FALSE)
})

# variable selection - CRD Analysis
output$crd_indepvar <- reactiveUI(function() {
  vars <- varnames()
  if(is.null(vars)) return()
  selectInput(inputId = "crd_indepvar", label = "Explanatory variables (select one or more):", 
              choices = vars[-match(input$crd_depvar, vars)] , selected = NULL, multiple = TRUE)
})

output$crd_mcp_testvar <- reactiveUI(function() {
  vars <- input$crd_indepvar
  if(is.null(vars)) return()
  selectInput(inputId = "crd_mcp_testvar", label = "Variables to test:", choices = vars, selected = NULL, multiple = TRUE)
})

ui_crdAnalysis <- function() {
  wellPanel(
    uiOutput("crd_depvar"),
    uiOutput("crd_indepvar"),
    checkboxInput(inputId = "crd_mcp", label = "MCP Tests", value = FALSE),
    conditionalPanel(condition = "input.analysistabs == 'Summary' && input.crd_mcp == true", 
                     uiOutput("crd_mcp_testvar"),
                     sliderInput('crd_mcp_sigLevel',"Significance level:", min = 0.85, max = 0.99, value = 0.95, step = 0.01)
    )
  )
}

summary.crdAnalysis <- function(result) {
  print(anova(result))
  if(input$crd_mcp) {
    cat("\n\n")
    mcp.crdAnalysis(result)
  }
}

plot.crdAnalysis <- function(result) {  
  if(input$crd_mcp && !is.null(input$crd_mcp_testvar)) {
    par(mfrow = c(3,2))
    plot(result, ask = FALSE)
    plot(result, which=4)
    plot_mcp.crdAnalysis(result)
  } else {
    par(mfrow = c(2,2))
    plot(result, ask = FALSE)
  }
}

extra.crdAnalysis <- function(result) {
  # nothing here yet, could put in test variance equality
  cat("Under development\n")
}

mcp.crdAnalysis <- function(result) {  
  if(!is.null(input$crd_mcp_testvar)) {
    cat("\nPost-Hoc Test (Tukey)\n\n")
    HSD.test(result, input$crd_mcp_testvar, group=TRUE,alpha=input$crd_mcp_sigLevel)
  }  
}

plot_mcp.crdAnalysis <- function(result) {
  if(input$crd_mcp && !is.null(input$crd_mcp_testvar)) {
    hsd <- HSD.test(result, input$crd_mcp_testvar, group=TRUE,alpha=input$crd_mcp_sigLevel)
    yl <- c(0, max(hsd$means) * 1.1)
    par(cex=1.2)
    bar.group(hsd$groups, ylim = yl, col="blue")
  } else {
    cat("Please select at least one variable for MCP tests.\n")
  }  
}

crdAnalysis <- reactive(function() {
  if(is.null(input$crd_indepvar)) return("Please select an explanatory variable")
  var1 <- input$crd_indepvar
  var2 <- input$crd_depvar
  
  dat <- getdata()[,c(var1,var2)]
  nIsFact <- sapply(var1, function(v) !is.factor(dat[[v]]))
  toFactor <- var1[nIsFact]

  if(length(toFactor) > 0) {    
    dat[,toFactor] <- lapply(toFactor, function(v) as.factor(dat[[v]]))
  }
  
  formula <- as.formula(paste(var2, "~", paste(var1, collapse = " + ")))
  aov(formula, data = dat, conf.level = input$crd_mcp_sigLevel)
})


# CRBD Analysis -----------------------------------------------------------

# variable selection - CRBD Analysis
output$crbd_blockvar <- reactiveUI(function() {
  vars <- varnames()
  if(is.null(vars)) return()
  selectInput(inputId = "crbd_blockvar", label = "Select block variable:", 
              choices = vars[-which(vars == input$crd_depvar)], selected = NULL, multiple = FALSE)
})

# variable selection - CRBD Analysis
output$crbd_indepvar <- reactiveUI(function() {
  vars <- varnames()
  if(is.null(vars)) return()
  selectInput(inputId = "crbd_indepvar", label = "Explanatory variables (select one or more):", 
              choices = vars[-match(c(input$crd_depvar, input$crbd_blockvar), vars)] , 
              selected = NULL, multiple = TRUE)
})

output$crbd_mcp_testvar <- reactiveUI(function() {
  vars <- input$crd_indepvar
  if(is.null(vars)) return()
  selectInput(inputId = "crbd_mcp_testvar", label = "Variables to test:", choices = vars, selected = NULL, multiple = TRUE)
})

ui_crbdAnalysis <- function() {
  wellPanel(
    uiOutput("crd_depvar"),
    uiOutput("crbd_blockvar"),
    uiOutput("crbd_indepvar"),
    checkboxInput(inputId = "crbd_mcp", label = "MCP Tests", value = FALSE),
    conditionalPanel(condition = "input.analysistabs == 'Summary' && input.crbd_mcp == true", 
                     uiOutput("crbd_mcp_testvar"),
                     sliderInput('crbd_mcp_sigLevel',"Significance level:", min = 0.85, max = 0.99, value = 0.95, step = 0.01)
    )
  )
}

summary.crbdAnalysis <- function(result) {
  print(anova(result))
  if(input$crbd_mcp) {
    cat("\n\n")
    mcp.crbdAnalysis(result)
  }
}

plot.crbdAnalysis <- function(result) {
  if(input$crbd_mcp && !is.null(input$crbd_mcp_testvar)) {
    par(mfrow = c(3,2))
    plot(result, ask = FALSE)
    plot(result, which=4)
    plot_mcp.crbdAnalysis(result)
  } else {
    par(mfrow = c(2,2))
    plot(result, ask = FALSE)
  }
}

extra.crbdAnalysis <- function(result) {
  # nothing here yet, could put in test variance equality
  cat("Under development\n")
}

mcp.crbdAnalysis <- function(result) {  
  if(!is.null(input$crbd_mcp_testvar)) {
    cat("\nPost-Hoc Test (Tukey)\n\n")
    HSD.test(result, input$crbd_mcp_testvar, group=TRUE,alpha=input$crbd_mcp_sigLevel)
  }  
}

plot_mcp.crbdAnalysis <- function(result) {
  if(input$crbd_mcp && !is.null(input$crbd_mcp_testvar)) {
    hsd <- HSD.test(result, input$crbd_mcp_testvar, group=TRUE,alpha=input$crbd_mcp_sigLevel)
    yl <- c(0, max(hsd$means) * 1.1)
    par(cex=1.2)
    bar.group(hsd$groups, ylim = yl, col="blue")
  } else {
    cat("Please select at least one variable for MCP tests.\n")
  }  
}

crbdAnalysis <- reactive(function() {
  if(is.null(input$crd_indepvar)) return("Please select an explanatory variable")
  var1 <- input$crbd_indepvar
  var2 <- input$crd_depvar
  var3 <- input$crbd_blockvar
  
  dat <- getdata()[,c(var1,var2,var3)]
  nIsFact <- sapply(c(var1,var3), function(v) !is.factor(dat[[v]]))
  toFactor <- c(var1,var3)[nIsFact]
  
  if(length(toFactor) > 0) {    
    dat[,toFactor] <- lapply(toFactor, function(v) as.factor(dat[[v]]))
  }
  
  formula <- as.formula(paste(var2, "~", var3, " + ", paste(var1, collapse = " + ")))
  aov(formula, data = dat, conf.level = input$crbd_mcp_sigLevel)
})


# FD Analysis -------------------------------------------------------------

# variable selection - CRBD Analysis
output$fd_blockvar <- reactiveUI(function() {
  vars <- varnames()
  if(is.null(vars)) return()
  selectInput(inputId = "fd_blockvar", label = "Select block variable:", 
              choices = c(None='.', as.list(vars[-which(vars == input$crd_depvar)])), selected = NULL, multiple = FALSE)
})

# variable selection - CRBD Analysis
output$fd_indepvar <- reactiveUI(function() {
  vars <- varnames()
  if(is.null(vars)) return()
  selectInput(inputId = "fd_indepvar", label = "Explanatory variables (select one or more):", 
              choices = vars[-match(c(input$crd_depvar, input$fd_blockvar), vars, nomatch=0)] , 
              selected = NULL, multiple = TRUE)
})

output$fd_mcp_testvar <- reactiveUI(function() {
  vars <- input$fd_indepvar
  if(is.null(vars)) return()
  selectInput(inputId = "fd_mcp_testvar", label = "Variables to test:", choices = vars, selected = NULL, multiple = TRUE)
})


ui_fdAnalysis <- function() {
  wellPanel(
    uiOutput("crd_depvar"),
    uiOutput("fd_blockvar"),
    uiOutput("fd_indepvar"),
    checkboxInput(inputId = "fd_mcp", label = "MCP Tests", value = FALSE),
    
    conditionalPanel(condition = "input.analysistabs == 'Summary' && input.fd_mcp == true", 
                     uiOutput("fd_mcp_testvar"),
                     sliderInput('fd_mcp_sigLevel',"Significance level:", min = 0.85, max = 0.99, value = 0.95, step = 0.01)
    )
  )
}

summary.fdAnalysis <- function(result) {
  print(anova(result))
  if(input$fd_mcp) {
    cat("\n\n")
    mcp.fdAnalysis(result)
  }
}

plot.fdAnalysis <- function(result) {  
  if(input$fd_mcp && !is.null(input$fd_mcp_testvar)) {
    par(mfrow = c(3,2))
    plot(result, ask = FALSE)
    plot(result, which=4)
    plot_mcp.fdAnalysis(result)
  } else {
    par(mfrow = c(2,2))
    plot(result, ask = FALSE)
  }
}

extra.fdAnalysis <- function(result) {
  # nothing here yet, could put in test variance equality
  cat("Under development\n")
}

fdAnalysis <- reactive(function() {
  if(is.null(input$fd_indepvar) || length(input$fd_indepvar) < 2 ) return("Please select two or more explanatory variables")
  var1 <- input$fd_indepvar
  var2 <- input$crd_depvar  
  var3 <- switch(input$fd_blockvar,
                 '.' = NULL,
                 input$fd_blockvar)
  
  dat <- getdata()[,c(var1,var2,var3)]
  nIsFact <- sapply(c(var1,var3), function(v) !is.factor(dat[[v]]))
  toFactor <- c(var1,var3)[nIsFact]
  
  if(length(toFactor) > 0) {    
    dat[,toFactor] <- lapply(toFactor, function(v) as.factor(dat[[v]]))
  }
  
  formula <- as.formula(paste(var2, "~", ifelse(is.null(var3), "", paste(var3, " + ")), paste(var1, collapse = " * ")))
  aov(formula, data = dat, conf.level = input$fd_mcp_sigLevel)
})

mcp.fdAnalysis <- function(result) {
  
  if(!is.null(input$fd_mcp_testvar)) {
    cat("\nPost-Hoc Test (Tukey)\n\n")
    HSD.test(result, input$fd_mcp_testvar, group=TRUE,alpha=input$fd_mcp_sigLevel)
  }

}

plot_mcp.fdAnalysis <- function(result) {
  if(input$fd_mcp && !is.null(input$fd_mcp_testvar)) {
    hsd <- HSD.test(result, input$fd_mcp_testvar, group=TRUE,alpha=input$fd_mcp_sigLevel)
    yl <- c(0, max(hsd$means) * 1.1)
    par(cex=1.2)
    bar.group(hsd$groups, ylim = yl, col="blue")
  } else {
    cat("Please select at least one variable for MCP tests.\n")
  }  
}