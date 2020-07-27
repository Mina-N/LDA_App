
# combine_data combines a list of dataframes, or grant application files, into one dataframe. This function
#   allows the user to upload grants from multiple years or multiple institutes and perform topic modeling on 
#   aggregated grant data.
# Input: data, a list of dataframes that contain grant information
# Output: df, a dataframe that contains aggregated grant information
combine_data <- function(data) {
    df <- data[[1]]
    
    if (length(data) > 1) {
        for (data_index in 2:length(data)) {
            tryCatch(
                {
                    df = rbind(df, data[[data_index]])
                },
                error = function(e) {
                    message("Input files do not have the same fields/column names.")
                    message(e)
                    return(NA)
                }
            )
        }
    }
    
    return(df)
}

# modify_data creates a text field and cleans the text for LDA. 
# Input: data, a dataframe with grant information
# Output: data, a dataframe with grant information primed for LDA
modify_data <- function(data) {
    # Rename columns
    names(data)[names(data) == 'PI Name'] <- 'PI.Name'
    names(data)[names(data) == 'PO Name'] <- 'PO.Name'
    names(data)[names(data) == 'RFA/PA Number'] <- 'RFA.PA.Number'                                                                                  
    names(data)[names(data) == 'Stat Desc'] <- 'Stat.Desc'                                                                                          
    names(data)[names(data) == 'Abstract Text (only)'] <- 'Abstract' 
    names(data)[names(data) == 'SA Text'] <- 'SA.Text'
    
    # Create text field
    text = c("Title", "Abstract", "SA.Text")
    data <- data[data$Abstract != "-",]
    data$Text <- data[[text[1]]]
    
    if(length(text) > 1){
        for(i in 2:length(text)){
            data$Text <- paste(data$Text, data[[text[i]]])
        }
    }
    
    data = select(data, -c("Title", "Abstract", "SA.Text"))
    data = mutate(data, Text = ifelse(is.na(Text), "", Text))
    data = filter(data, Text != "")
    
    # Remove weird characters
    data$Text = str_replace_all(data$Text, "\n", " ")
    data$Text = str_replace_all(data$Text, "\r", " ")
    data$Text = str_replace_all(data$Text, "'", "")
    data$Text = str_replace_all(data$Text, "-", " ")
    
    # Replace non-alpha numeric characters with a space
    data$Text = str_replace_all(data$Text, "[^abcdefghijklmnopqrstuvwxyzABCDEFHIJKLMNOPQRSTUVWXZ0123456789 ]", " ")
    
    # Put everything in lowercase
    data$Text = tolower(data$Text)
    
    # Remove a few select words and extra spaces
    data$Text = str_replace_all(data$Text, "abstract", " ")
    data$Text = str_replace_all(data$Text, "project", " ")
    data$Text = str_replace_all(data$Text, "proposal", " ")
    data$Text = str_replace_all(data$Text, "summary", " ")
    data$Text = str_replace_all(data$Text, "narrative", " ")
    data$Text = str_replace_all(data$Text, "background", " ")
    data$Text = str_replace_all(data$Text, "supplement", " ")
    data$Text = str_replace_all(data$Text, "significance", " ")
    data$Text = str_replace_all(data$Text, "overall", " ")
    data$Text = str_replace_all(data$Text, "title", " ")
    data$Text = str_replace_all(data$Text, "specific aims", " ")
    data$Text = str_replace_all(data$Text, "goal", " ")
    data$Text = str_replace_all(data$Text, "research plan", " ")
    data$Text = str_replace_all(data$Text, "overview", " ")
    data$Text = str_replace_all(data$Text, "\\s+", " ")
    data$Text = str_replace_all(data$Text, "description provided by applicant", " ")
    
    # Trim spaces on both sides of text
    data$Text = str_trim(data$Text, side = "both")
    
    
    # Make text unique
    data <- data[!duplicated(data[ , c("Text")]),]
    
    return(data)
}

# dt_matrix_vocab creates a document-term matrix from vocabulary that has filtered out stopwords.
# Input: data, a dataframe with clean text
#        stopwords, a dataframe of words to exclude from the topic modeling analysis
# Output: dtm, a document-term matrix
dt_matrix_vocab <- function(data, stopwords) {
    setDT(data)
    setkey(data, Project)
    
    stem_tokenizer =function(x) {
        word_tokenizer(x) %>% lapply(SnowballC::wordStem, language="en") 
    }
    
    it = itoken(data$Text,
                #tokenizer = word_tokenizer, 
                tokenizer = stem_tokenizer,
                ids = data$Project, 
                progressbar = FALSE)
    
    stop = stopwords(kind = "SMART")
    stop = str_replace_all(stop, "'", "")
    
    vocab = create_vocabulary(it, stopwords = stop, ngram = c(1L, 2L))
    
    vocab = prune_vocabulary(vocab,
                             term_count_min = 10)

    if (!is.null(stopwords)) {
        vocab = filter(vocab, !(term %in% stopwords))
    }

    vectorizer = vocab_vectorizer(vocab)
    dtm = create_dtm(it, vectorizer)
    
    return(dtm)
}

# optimize_param calculates the perplexity of topic groups, which is a function of different combinations of parameters alpha, beta, and topic number.
#   It then writes these parameters and their associated perplexity to a file. 
# Input: dtm, a document-term matrix
# Output: filename, the name of the file to which parameters are written to
optimize_param <- function(dtm) {
    # TODO: give user the ability to change range of parameters
    as = c(0.05, 10^(-5:5))
    bs = c(0.05, 10^(-5:5))
    topics = 3:15
    
    pp = NULL
    
    for(topic in topics){
        a1 = 50/topic # Default Values
        b1 = 1/topic  # Default Values
        lda_model = LDA$new(doc_topic_prior = a1, 
                            topic_word_prior = b1,
                            n_topics = topic)
        
        set.seed(123)
        doc_topic_distr = 
            lda_model$fit_transform(x = dtm, 
                                    n_iter = 1000, 
                                    convergence_tol = 0.0001, 
                                    n_check_convergence = 25, 
                                    progressbar = FALSE,
                                    verbose = FALSE)
        
        topic_word_distr = lda_model$topic_word_distribution
        
        p1 = perplexity(dtm, topic_word_distr, doc_topic_distr)
        
        
        for(a in as){
            for(b in bs){
                lda_model = LDA$new(doc_topic_prior = a, 
                                    topic_word_prior = b,
                                    n_topics = topic)
                
                set.seed(123)
                doc_topic_distr = 
                    lda_model$fit_transform(x = dtm, 
                                            n_iter = 1000, 
                                            convergence_tol = 0.0001, 
                                            n_check_convergence = 25, 
                                            progressbar = FALSE,
                                            verbose = FALSE)
                
                topic_word_distr = lda_model$topic_word_distribution
                
                p = perplexity(dtm, topic_word_distr, doc_topic_distr)
                # TODO: Check coherence
                
                print(paste(topic, a, b, p))
                
                if(p < p1){
                    a1 = a
                    b1 = b
                    p1 = p
                }
            }
        }
        
        pp1 = data.frame(Topics = topic, Alpha = a1, Beta = b1, Perplexity = p1)
        pp = rbind(pp, pp1)
    }
    
    # Write LDA parameters to file 
    # TODO: give users the ability to change output file name
    filename <- "LDA_Param.csv"
    write_csv(pp, filename)
    return(filename)
}

# acronym_lookup replaces acronyms in topic groups with their derivations. These acronyms are unique to NINR.
# Input: topic_words, a collection of topics and associated words
# Output: topic_words, a collection of topics and associated words with derivations in place of acronyms
acronym_lookup <- function(topic_words) {
    acronym_lookup <- data.frame("Acronym" = c("pa", "smm", "hf", "plwh", "t2dm", "msm", "fsw", "hana", "acp", "cds", "cqa", "cci", "comt", "bpsd", "pwd", "fc", "eolpc", "pcrc", "cmc", "4_bpsd", "pes", "mcc",
                                               "hhc", "dts", "cri", "vte", "pru", "cbpr", "cbt", "trkb", "lbp", "trkb_t1", "oa", "eol"), 
                                 "Derivation" = c("physical activity", "severe maternal morbidity ", "heart failure", "persons living with HIV", "type 2 diabetes mellitus", "men who have sex with men", "female
                                              sex worker", "HIV-associated non-AIDS conditions", "advance care planning", "clinical decision support", "communication quality analysis", "chronic critical
                                              illness", "catechol-O-methyltransferase", "behavioral and psychological symptoms of dementia", "persons with dementia", "fecal calprotectin", "end of life and
                                              palliative care", "Palliative Care Research Consortium", "children with medical complexity", "behavioral and psychological symptoms of dementia", "patient
                                              engagement specialist", "multiple chronic conditions", "home healthcare", "disinfection tracking system", "cardiorespiratory instability", "Venous
                                              thromboembolism", "pruritis", "community-based participatory research", "cognitive behavioral therapy", "tropomyosin-related receptor kinase type B", "lower
                                              back pain", "truncated isoform of Trkb", "osteoarthritis", "end of life"))                                                                                                                                                         
    
    for (row in 1:nrow(topic_words)) {
        for (col in 1:ncol(topic_words)) {
            for (row_acro in 1:nrow(acronym_lookup)) {
                if (as.String(topic_words[row, col]) == as.String(acronym_lookup[row_acro, 1])) {
                    topic_words[row, col] <- as.String(acronym_lookup[row_acro, 2])
                }
            }
        }
    }
    
    return(topic_words)
}

# calculate_term_freq calculates the frequency with which words in topic groups appear in the corpus and writes this information to a file.
# Input: topic_words, a collection of topics and associated words
#        dtm, a document-term matrix
calculate_term_freq <- function(topic_words, dtm) {
    find_dtm <- function(word, word_matrix) {
        cond = 0
        freq = 0
        for (col in colnames(word_matrix)) {
            if (grepl(word, col)) {
                cond = 1
                freq <- freq + sum(word_matrix[, col])
            }
        }   
        return(list(cond=cond, freq=freq))
    }
    
    term_frequency_pre_LDA <- topic_words
    for (row in 1:nrow(term_frequency_pre_LDA)) {
        for (col in 1:ncol(term_frequency_pre_LDA)) {
            term <- find_dtm(term_frequency_pre_LDA[row, col], dtm)
            if (term$cond == 1) {
                term_frequency_pre_LDA[row, col] <-  paste(term_frequency_pre_LDA[row, col], as.String(term$freq), sep= ", ")
            }
        }
    }
    
    write_csv(term_frequency_pre_LDA, "topics_term_frequency.csv")
}

# topic_term_freq_space creates scatter plots with axes in term frequency space.
# Input: dtm, a document-term matrix
#        grants, a dataframe with grant information
# Output: n2, a list containing column headers of topic percentages
topic_term_freq_space <- function(dtm, grants) {
    n = names(grants)
    n1 = names(select(grants, Project:Topic2))
    n2 = sort(n[!(n %in% n1)])
    
    dtm2 = as.matrix(dtm)
    dtm3 = data.frame(dtm2)
    dtm3$Project = row.names(dtm2)
    dtm4 = inner_join(select(grants, Project, Topic1, PO.Name), dtm3)
    
    set.seed(123)
    
    ts = Rtsne(as.matrix(select(dtm4, -Project, -Topic1, -PO.Name)), check_duplicates=FALSE, pca=TRUE, perplexity=30, theta=0.5, dims=2)
    dts = as.data.frame(ts$Y)
    dts2 = cbind(select(dtm4, Project, Topic1, PO.Name), dts)
    #dts2 = rename(dts2, PO.Name = PO.Name)
    
    topic_term <- (ggplot(dts2, aes(x = V1, y = V2, col = Topic1))
                   + geom_point()
                   + ggtitle("Most Likely Topic of Each Grant in Term Frequency Space")
                   + theme_minimal()
                   + theme(legend.position = "bottom",
                        plot.title = element_text(hjust = 0.5),
                        axis.text.x = element_text(angle = 0, vjust = 0.4)))
    

    ggsave(file="topic_term_freq_space.png", plot = topic_term)
    
    
    PO_term <- (ggplot(dts2, aes(x = V1, y = V2, col = PO.Name))
                + geom_point()
                + ggtitle("PO of Each Grant in Term Frequency Space")
                + theme_minimal()
                + theme(legend.position = "bottom",
                    plot.title = element_text(hjust = 0.5),
                    axis.text.x = element_text(angle = 0, vjust = 0.4)))
    
    ggsave(file="PO_term_freq_space.png", plot = topic_term)
    
    set.seed(123)
    
    ts = Rtsne(as.matrix(select(grants, n2)), check_duplicates=FALSE, pca=TRUE, perplexity=30, theta=0.5, dims=2)
    dts = as.data.frame(ts$Y)
    dts2 = cbind(select(grants, Project, Topic1, PO.Name), dts)
    #dts2 = rename(dts2, PO.Name = PO.Name)
    
    topic_topic <- (ggplot(dts2, aes(x = V1, y = V2, col = Topic1)) 
                    + geom_point()
                    + ggtitle("Most Likely Topic of Each Grant in Topic Space")
                    + theme_minimal()
                    + theme(legend.position = "bottom",
                        plot.title = element_text(hjust = 0.5),
                        axis.text.x = element_text(angle = 0, vjust = 0.4)))

    ggsave(file="topic_topic_space.png", plot = topic_term)
    
    PO_topic <- (ggplot(dts2, aes(x = V1, y = V2, col = PO.Name)) 
                    + geom_point()
                    + ggtitle("PO of Each Grant in Topic Space")
                    + theme_minimal()
                    + theme(legend.position = "bottom",
                        plot.title = element_text(hjust = 0.5),
                        axis.text.x = element_text(angle = 0, vjust = 0.4)))
    
    ggsave(file="PO_topic_space.png", plot = topic_term)
    
    return(n2)
}

# topic_PO creates plots of the breakdown of topic frequency by PO and saves them to a file.
# Input: grants, a dataframe of grant information
topic_PO <- function(grants) {
    grants_mod = (
        grants %>% group_by(PO.Name)
        %>% mutate(N = n())
        %>% ungroup()
        %>% group_by(PO.Name, Topic1, N)
        %>% summarize(n = n())
        %>% ungroup()
        %>% mutate(Percentage = round(100*n/N))
    )
    
    PO_freq <- (
        ggplot(grants_mod, aes(x = PO.Name, y = n, fill = Topic1))
        + geom_bar(stat = "identity", width = 0.5)
        + ylab("Frequency")
        + xlab("PO")
        + ggtitle("Most Likely Topics of POs (Frequency in Portfolio)")
        #+ scale_y_continuous(labels = scales::percent)
        + theme_minimal()
        + theme(legend.position = "right",
                plot.title = element_text(hjust = 0.5),
                axis.text.x = element_text(angle = 90, vjust = 0.4)) 
    )
    
    ggsave(file="Topic_Frequency_PO.png", plot = PO_freq)
    
    PO_percent <- (
        ggplot(grants_mod, aes(x = PO.Name, y = Percentage/100, fill = Topic1))
        + geom_bar(stat = "identity", width = 0.5)
        + ylab("Percentage")
        + xlab("PO")
        + ggtitle("Most Likely Topics of POs (Percentage of Portfolio)")
        + scale_y_continuous(labels = scales::percent)
        + theme_minimal()
        + theme(legend.position = "right",
                plot.title = element_text(hjust = 0.5),
                axis.text.x = element_text(angle = 90, vjust = 0.4)) 
    )
    
    ggsave(file="Topic_Percentage_PO.png", plot = PO_percent)
}

# topic_perc_dist creates plots that display the distribution of topic percentages across the entire corpus and
#   the distribution of topic percentages by PO.
# Input: grants, a dataframe of grant information
# Output: grants_mod, a dataframe that contains the percentage breakdown of topics for each grant
topic_perc_dist <- function(grants) {
    grants_mod = (
        grants %>% select(PO.Name, n2)
        %>% melt(measure.vars = n2, variable.name = "Topic", value.name = "Fraction")
        %>% arrange(PO.Name)
    )
    
    topic_dist = (
        ggplot(grants_mod, aes(x = Topic, y = Fraction, fill = Topic))
        + geom_boxplot(width = 0.8, outlier.shape = 1)
        + stat_summary(fun.y = "mean", geom = "point", size = 0.9)
        + ylab("Percentage")
        + xlab("Topic")
        + ggtitle("Distribution of Topic Percentages")
        + scale_y_continuous(labels = scales::percent)
        + theme_minimal()
        + theme(legend.position = "none",
                plot.title = element_text(hjust = 0.5),
                axis.text.x = element_text(angle = 0, vjust = 0.4)) 
    )
    
    ggsave(file="Topic_Percentage_Distribution.png", plot = topic_dist)
    
    topic_dist1 = ggplot_build(topic_dist)
    colors = unique(topic_dist1$data[[1]]["fill"])
    colors$Topic = sort(unique(grants_mod$Topic))
    
    topic_dist_PO = (
        ggplot(grants_mod, aes(x = PO.Name, y = Fraction, fill = Topic))
        + geom_boxplot(width = 0.8, outlier.shape = 1)
        + stat_summary(fun.y = "mean", geom = "point", position = position_dodge(width=0.8), size = 0.9)
        + ylab("Percentage")
        + xlab("PO")
        + ggtitle("Distribution of Topic Percentages by PO")
        + scale_y_continuous(labels = scales::percent)
        + theme_minimal()
        + theme(legend.position = "right",
                plot.title = element_text(hjust = 0.5),
                axis.text.x = element_text(angle = 90, vjust = 0.4)) 
    )
    
    ggsave(file="Topic_Percentage_Distribution_PO.png", plot = topic_dist_PO)
    
    return(grants_mod)
}

# topic_dist_PO creates separate plots for each topic, where each plot shows the percentage distribution of a topic by PO
# Input: grants, a dataframe of grant information
topic_dist_PO <- function(grants) {
    
    for(topic in sort(unique(grants$Topic))){
        g = (
            ggplot(filter(grants, Topic == topic), aes(x = PO.Name, y = Fraction))
            + geom_boxplot(width = 0.8, fill = colors[colors$Topic == topic, "fill"], outlier.shape = 1)
            + stat_summary(fun.y = "mean", geom = "point", size = 0.9)
            + ylab("Percentage")
            + xlab("PO")
            + scale_y_continuous(limits = c(0,1), labels = scales::percent)
            + ggtitle(paste("Distribution of Topic", topic, "by PO"))
            + theme_minimal()
            + theme(legend.position = "right",
                    plot.title = element_text(hjust = 0.5),
                    plot.subtitle = element_text(hjust = 0.5),
                    #axis.text.x = element_text(color = s$Color),
                    panel.grid.major.x = element_blank(),
                    axis.text.x = element_text(angle = 90, vjust = 0.4))
        )
        
        ggsave(file = paste("Topic", topic, "_Dist_PO.png"), plot = g)
        
    }
}

# output_topics writes grants and their topic percent breakdown to a file.
# Input: grants, a dataframe with grant information
# Output: grants_mod, a dataframe with topic breakdown in the form of percentages.
output_topics <- function(grants) {
    grants_mod = (
        grants %>% mutate(
            T1 = round(100*T1, 2),
            T2 = round(100*T2, 2),
            T3 = round(100*T3, 2),
            T4 = round(100*T4, 2),
            T5 = round(100*T5, 2),
            T6 = round(100*T6, 2),
            T7 = round(100*T7, 2),
            T8 = round(100*T8, 2),
            T9 = round(100*T9, 2),
            T10 = round(100*T10, 2),
            T11 = round(100*T11, 2),
            T12 = round(100*T12, 2),
            T13 = round(100*T13, 2))
    )
    
    write_csv(grants_mod, "topic_composition.csv")
    
    return(grants_mod)
}

# ggupset_plot creates plots that show the frequency of topic intersections among grants.
# Input: grants, a dataframe with grant information
#        data, a dataframe with original grant information
#        topic_columns, a list containing column headers of topic percentages
ggupset_plot <- function(grants, data, topic_columns) {
    topic_list <- function(x) {
        top_list <- c()
        index = 0
        row <- x[topic_columns]
        for (i in 1: length(row)) {
            if (as.double(row[i]) >= 33.33) {
                top_list <- append(top_list, topic_columns[i])
                index = index + 1
            }
        }
        return(top_list)
    }
    
    vals <- apply(grants, 1, function(x) topic_list(x))
    
    grants$FY <- data$FY
    
    if (!is.null(vals)) {
        grants$TopicList <- vals
        grants_exclude <- grants[grants$TopicList == 'NULL',]
        
        grants_mod = (
            grants %>%
            distinct(Project, .keep_all=TRUE) %>%
            ggplot(aes(x=TopicList)) +
            geom_bar() +
            scale_x_upset(order_by = "freq")
        )
        
        ggsave(file = "ggupset.png", plot = grants_mod)
    }
}

# run_lda executes the LDA algorithm and writes output to the app directory.
# Input: lda_param, a dataframe that contains LDA parameters that have already been optimized for the corpus
#        dtm, a document-term matrix
#        data, a dataframe that contains grant information
#        lambda, a value between 0 and 1 that determines whether word groupings consist of words
#           strongly associated with topics or words that are most predictive for topics.
run_lda <- function(lda_param, dtm, data, lambda) {
    # Load Optimal Parameters
    pp = lda_param
    pp = data.frame(pp)
    
    # TODO: give users the option to change the number of topics
    topic = 13
    
    pp1 = filter(pp, Topics == topic)
    a1 = pp1$Alpha
    b1 = pp1$Beta
    t1 = pp1$Topics
    
    lda_model = LDA$new(doc_topic_prior = a1, 
                        topic_word_prior = b1,
                        n_topics = t1)
    
    set.seed(123)
    doc_topic_distr = 
        lda_model$fit_transform(x = dtm, 
                                n_iter = 1000, 
                                convergence_tol = 0.0001, 
                                n_check_convergence = 25, 
                                progressbar = FALSE,
                                verbose = FALSE)
    
    topic_word_distr = lda_model$topic_word_distribution
    
    d = as.matrix(doc_topic_distr)
    d = data.frame(Project = row.names(d), d)
    colnames(d) = str_replace_all(names(d), "X", "T")
    
    for(i in 1:dim(d)[1]){
        temp = names(sort(d[i, 2:dim(d)[2]], decreasing = TRUE))[1:2]           
        d[i, "Topic1"] = temp[1]
        d[i, "Topic2"] = temp[2]
    }
    
    l = dim(d)[2]
    d = d[, c(1, l-1,l, 4:l-2)]
    
    k = lda_model$get_top_words(n = 20, topic_number = 1:t1, lambda = lambda)
    
    k1 = data.frame(k)
    colnames(k1) = paste0("Topic", 1:t1)
    
    # TODO: allow user to enter their own acronyms
    k = acronym_lookup(k)
    
    # TODO: allow user to customize directory
    lda_model$plot(out.dir = "lda_model", open.browser = FALSE)
    
    j = read_json("lda_model/lda.json")
    j = j$topic.order
    j = as.integer(j)
    
    k = k[, j]
    k = as.data.frame(k)
    
    write_csv(k, "topic_group_words.csv")
    
    # Select necessary columns 
    new_data = inner_join(select(data, Project:PO.Name), d)
    
    # Output 2D scatter plots (optional) **
    topic_columns <- topic_term_freq_space(dtm, new_data)
    
    # Output most likely topics of POs (optional) **
    topic_PO(new_data)
    
    # Output distribution of topics (optional) **
    grants <- topic_perc_dist(new_data)
    
    # Output distribution of topic by PO (optional) **
    topic_dist_PO(grants)
    
    # TODO: allow users to modify number of topics/output filename
    # Output Grants and Topic Distribution (optional) **
    topic_comp <- output_topics(new_data)
    
    # Output ggupset plot (optional) **
    ggupset_plot(topic_comp, data, topic_columns)
}






