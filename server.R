library(shiny);
library(RColorBrewer);
library(ggplot2);
library(stringr); # for str_wrap
library(gender);
library(RTextTools);
library(wordcloud);

# Initialise
d <- read.csv("summary_of_results_2015_app_round_160322.csv",
              stringsAsFactors = FALSE);
load(file = "journals_2015.rda");
journals <- journals[which(sapply(journals, length) < 100)];
tdm <- as.data.frame(as.matrix(create_matrix(
    d$Plain.Description,
    language = "english",
    removeStopwords = TRUE,
    removeNumbers = TRUE,
    stemWords = TRUE,
    removeSparseTerms = 0.9))); 


shinyServer(function(input, output, session) {

    output$institutionPlot <- renderPlot({
        # Remove NAs, N/As, and empty strings
        keep <- which(
            d$Admin.Institution != "" &
            d$Admin.Institution != "NA" &
            d$Admin.Institution != "N/A");
        data <- d[keep, ];
        # Set colors
        nInstitution <- length(unique(data$Admin.Institution));
        col <- colorRampPalette(brewer.pal(8, "Dark2"))(nInstitution);
        names(col) <- sort(unique(data$Admin.Institution));
        # Filter based on grant type
        if (input$grantType != "All") {
            data <- data[which(d$Grant.Type == input$grantType), ];
        }
        # Select institution from data and make frequency table
        df <- as.data.frame(table(data$Admin.Institution));
        if (input$minFreq > 0) {
            df <- df[which(df$Freq >= input$minFreq), ];
        }
        # Plot
        if (input$asPercentage == TRUE) {
            df$Freq <- df$Freq / sum(df$Freq) * 100;
        }
        df$Fill <- col[match(df$Var1, names(col))];
        df$Var1 <- factor(df$Var1, levels = rev(levels(df$Var1)));
        gg <- ggplot(df, aes(x = Var1, y = Freq, fill = Var1));
        gg <- gg + geom_bar(stat = "identity");
        gg <- gg + scale_fill_manual(values = rev(df$Fill));
        gg <- gg + coord_flip();
        gg <- gg + xlab("Institution");
        if (input$asPercentage == TRUE) {
            gg <- gg + ylab("Percentage of grants");
        } else {
            gg <- gg + ylab("Number of grants");
        }
        gg <- gg + theme_bw();
        gg <- gg + theme(axis.text = element_text(size = input$fontLabel),
                         axis.title.x = element_text(size = input$fontAxes),
                         axis.title.y = element_text(size = input$fontAxes));
        gg <- gg + guides(fill = FALSE);
        print(gg);
    })

    output$institutionPlotUI <- renderUI({
        plotOutput("institutionPlot", height = input$canvasHeight)
    })

    output$genderPlot <- renderPlot({
        # Remove NAs, N/As, and empty strings
        keep <- which(
            d$CIA_Name != "" &
            d$CIA_Name != "NA" &
            d$CIA_Name != "N/A");
        data <- d[keep, ];
        # Set colors
        nGender <- 3;
        col <- colorRampPalette(brewer.pal(8, "Dark2"))(nGender);
        names(col) <- c("female", "male", "unknown");
        # Filter based on grant type
        if (input$grantType != "All") {
            data <- data[which(d$Grant.Type == input$grantType), ];
        }
        # Genderise and add gender column to data
        firstname <- sapply(strsplit(data$CIA_Name, " "), function(x) x[2]);
        gender <- as.data.frame(gender(firstname));
        data <- cbind(data,
                      Gender = gender[match(firstname, gender$name), 4],
                      stringsAsFactors = FALSE);
        data$Gender[is.na(data$Gender)] <- "unknown";
        # Select gender from data and make frequency table
        df <- as.data.frame(table(data$Gender));
        if (input$minFreq > 0) {
            df <- df[which(df$Freq > input$minFreq), ];
        }
        # Plot
        if (input$asPercentage == TRUE) {
            df$Freq <- df$Freq / sum(df$Freq) * 100;
        }
        df$Fill <- col[match(df$Var1, names(col))];
        df$Var1 <- factor(df$Var1, levels = rev(levels(df$Var1)));
        gg <- ggplot(df, aes(x = Var1, y = Freq, fill = Var1));
        gg <- gg + geom_bar(stat = "identity");
        gg <- gg + scale_fill_manual(values = rev(df$Fill));
        gg <- gg + coord_flip();
        gg <- gg + xlab("Gender");
        if (input$asPercentage == TRUE) {
            gg <- gg + ylab("Percentage of grants");
        } else {
            gg <- gg + ylab("Number of grants");
        }
        gg <- gg + theme_bw();
        gg <- gg + theme(axis.text = element_text(size = input$fontLabel),
                         axis.title.x = element_text(size = input$fontAxes),
                         axis.title.y = element_text(size = input$fontAxes));
        gg <- gg + guides(fill = FALSE);
        print(gg);
    })
    
    output$genderPlotUI <- renderUI({
        plotOutput("genderPlot", height = input$canvasHeight)
    })

    output$keywordPlot <- renderPlot({
        # Filter based on grant type
        data <- d;
        if (input$grantType != "All") {
            data <- data[which(d$Grant.Type == input$grantType), ];
        }

        # Select keywords from data
        kw <- data[, grep("KW", colnames(data))];
        kwlist <- lapply(split(kw, seq(nrow(kw))), as.character);
        kwlist <- lapply(kwlist, function(x) x[which(x != "")]);
        # Make frequency table
        df <- as.data.frame(table(unlist(kwlist)));
        if (input$minFreq > 0) {
            df <- df[which(df$Freq > input$minFreq), ];
        }
        # Set colors
        nKW <- nrow(df);
        col <- colorRampPalette(brewer.pal(8, "Dark2"))(nKW);
        names(col) <- sort(df$Var1);
        # Plot
        if (input$asPercentage == TRUE) {
            df$Freq <- df$Freq / sum(df$Freq) * 100;
        }
        df$Var1 <- factor(df$Var1, levels = rev(levels(df$Var1)));
        df$Fill <- col[match(df$Var1, names(col))];
        gg <- ggplot(df, aes(x = Var1, y = Freq, fill = Var1));
        gg <- gg + geom_bar(stat = "identity");
        gg <- gg + scale_fill_manual(values = rev(df$Fill));
        gg <- gg + coord_flip();
        gg <- gg + xlab("Keyword");
        if (input$asPercentage == TRUE) {
            gg <- gg + ylab("Percentage of all keywords");
        } else {
            gg <- gg + ylab("Number of occurance");
        }
        gg <- gg + theme_bw();
        gg <- gg + theme(axis.text = element_text(size = input$fontLabel),
                         axis.title.x = element_text(size = input$fontAxes),
                         axis.title.y = element_text(size = input$fontAxes));
        gg <- gg + guides(fill = FALSE);
        print(gg);
    })

    output$keywordPlotUI <- renderUI({
        plotOutput("keywordPlot", height = input$canvasHeight)
    })

    output$publicationPlot <- renderPlot({
        data <- d;
        if (input$grantType != "All") {
            data <- data[which(d$Grant.Type == input$grantType), ];
        }
        # Select journals for relevant authors
        sel <- names(journals) %in% data$CIA_Name;
        df <- as.data.frame(table(unlist(journals[sel])));
        if (input$minFreq > 0) {
            df <- df[which(df$Freq > input$minFreq), ];
        }
        # Set colors
        nJournals <- nrow(df);
        col <- colorRampPalette(brewer.pal(8, "Dark2"))(nJournals);
        names(col) <- sort(df$Var1);
        # Plot
        if (input$asPercentage == TRUE) {
            df$Freq <- df$Freq / sum(df$Freq) * 100;
        }
        df$Var1 <- factor(df$Var1, levels = rev(levels(df$Var1)));
        df$Fill <- col[match(df$Var1, names(col))];
        gg <- ggplot(df, aes(x = Var1, y = Freq, fill = Var1));
        gg <- gg + geom_bar(stat = "identity");
        gg <- gg + scale_fill_manual(values = rev(df$Fill));
        gg <- gg + coord_flip();
        gg <- gg + xlab("Journal");
        if (input$asPercentage == TRUE) {
            gg <- gg + ylab("Percentage of manuscripts");
        } else {
            gg <- gg + ylab("Number of manuscripts");
        }
        gg <- gg + theme_bw();
        gg <- gg + theme(axis.text = element_text(size = input$fontLabel),
                         axis.title.x = element_text(size = input$fontAxes),
                         axis.title.y = element_text(size = input$fontAxes));
        gg <- gg + guides(fill = FALSE);
        gg <- gg + scale_x_discrete(labels = function(x) str_wrap(x, width = 100));
        print(gg);
    })

    output$publicationPlotUI <- renderUI({
        plotOutput("publicationPlot", height = input$canvasHeight)
    })

    output$publicationsPerGrantPlot <- renderPlot({
        # Split data based on grant type and keep grant type categories
        # with more than input$minFreq entries
        data <- split(d, d$Grant.Type);
        if (input$minFreq > 0) {
            data <- data[sapply(data, function(x) nrow(x) > input$minFreq)];
        }
        # Select journals for every author in every grant type category
        data <- sapply(data, function(x)
                       journals[names(journals) %in% x$CIA_Name]);
        lst <- sapply(data, function(x) sapply(x, length));
        if (input$missingPubsAsZero == FALSE) {
            lst <- lapply(lst, function(x) x[x > 0]);
        }
        # Set colors
        nGrantType <- length(lst);
        col <- colorRampPalette(brewer.pal(8, "Dark2"))(nGrantType);
        names(col) <- names(lst);
        # Plot
        df <- data.frame(x = unlist(lst),
                         grp = rep(names(lst), times = sapply(lst, length)));
        df$grp <- factor(df$grp, levels = rev(levels(df$grp)));
        gg <- ggplot(df, aes(x = grp, y = x, fill = grp));
        gg <- gg + geom_violin(trim = TRUE);
        gg <- gg + geom_boxplot(width = 0.1, fill = "white");
        gg <- gg + scale_fill_manual(values = col);
        gg <- gg + coord_flip();
        gg <- gg + xlab("Grant type");
        gg <- gg + ylab("Number of publications per author (2010 - 2015)");
        gg <- gg + theme_bw();
        gg <- gg + theme(axis.text = element_text(size = input$fontLabel),
                         axis.title.x = element_text(size = input$fontAxes),
                         axis.title.y = element_text(size = input$fontAxes));
        gg <- gg + guides(fill = FALSE);
        print(gg);
    })

    output$publicationsPerGrantPlotUI <- renderUI({
        plotOutput("publicationsPerGrantPlot", height = input$canvasHeight);
    })


    output$wordcloudPlot <- renderPlot({
        # Split data based on grant type and keep grant type categories
        # with more than input$minFreq entries
        data <- split(tdm, d$Grant.Type);
#        if (input$minFreq > 0) {
#            data <- data[sapply(data, function(x) nrow(x) > input$minFreq)];
#        }
        if (input$grantType != "All") {
            freq <- colSums(data[[which(names(data) == input$grantType)]]);
        } else {
            freq <- colSums(tdm);
        }
        wordcloud(names(freq), freq, 
                  min.freq = 3, max.words = 200, 
                  colors = brewer.pal(8, "Dark2"),
                  scale = c(8, 0.2));
    })

    output$wordcloudPlotUI <- renderUI({
        plotOutput("wordcloudPlot", height = input$canvasHeight);
        })
    
})

