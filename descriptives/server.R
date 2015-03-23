source("global.R")
shinyServer(function(input, output) {

get.data <- reactive({
    ret.data <- .ts.data
    ret.data <- ret.data[(Transaction_Date >= as.POSIXct(input$date_range[[1]])) &
                       (Transaction_Date <= as.POSIXct(input$date_range[[2]]))]
	return(ret.data)
	})

output$h_log_budget <- renderPlot({
    ts.data <- get.data()
    to.plot <- unique(ts.data[,list(Budget=Budget),by=Org])

    ggplot(to.plot,aes(x=log(Budget))) + geom_histogram(binwidth=0.1) +
        annotate("text",x=Inf,y=Inf,
                 label=paste0("Avg: ",sprintf("%2.1g",mean(to.plot$Budget))," (SD: ",sprintf("%2.1g",sd(to.plot$Budget)),")"),
                     vjust=1.2,hjust=1.05,size=6)
})
output$h_log_liscenses <- renderPlot({
    ts.data <- get.data()
    to.plot <- unique(ts.data[,list(n_licenses=sum(Licenses)),by=Order_Num])

    ggplot(to.plot,aes(x=log(n_licenses))) + geom_histogram(binwidth=1) +
        xlab("log(# Licenses / Order)")

})

output$gr_log_budget_value <- renderPlot({
    ts.data <- get.data()

    ggplot(ts.data,aes(x=log(Budget),y=Value)) + geom_point()
})
output$gr_log_budget_revenue <- renderPlot({
    ts.data <- get.data()

    ggplot(ts.data,aes(x=log(Budget),y=Revenue)) + geom_point()
})

output$h_n_items_per_org <- renderPlot({
    ts.data <- get.data()
    to.plot <- unique(ts.data[,list(n_items=length(unique(Item))),by=Org])
    ggplot(to.plot,aes(x=n_items)) + geom_histogram(binwidth=1) +
        xlab("Items/Org")
})

output$h_log_n_items_per_vendor <- renderPlot({
    ts.data <- get.data()

    to.plot <- unique(ts.data[,list(n_items=length(unique(Item))),by=Vendor])
#    to.plot <- to.plot[order(n_items)]
    ggplot(to.plot,aes(x=log(n_items))) + geom_histogram(binwidth=0.2) +
    xlab("log(# unique items / vendor)")

})

output$h_log_n_licenses_per_org <- renderPlot({
    ts.data <- get.data()
    to.plot <- unique(ts.data[,list(n_licenses=log(sum(Licenses))),by=Org])
    ggplot(to.plot,aes(x=n_licenses)) + geom_histogram(binwidth=0.2) +
        xlab("log(# licenses/org)")
})
output$h_log_n_licenses_per_vendor <- renderPlot({
    ts.data <- get.data()
    to.plot <- unique(ts.data[,list(n_licenses=log(sum(Licenses))),by=Vendor])
    ggplot(to.plot,aes(x=n_licenses)) + geom_histogram(binwidth=0.2) +
        xlab("log(# licenses/vendor)")
})

output$h_log_n_orders_per_org <- renderPlot({
    ts.data <- get.data()
    to.plot <- unique(ts.data[,list(n_orders=length(unique(Order_Num))),by=Org])
    ggplot(to.plot,aes(x=n_orders)) + geom_histogram(binwidth=1) +
        xlab("# orders/org")
})
output$h_log_n_orders_per_vendor <- renderPlot({
    ts.data <- get.data()
    to.plot <- unique(ts.data[,
                              list(n_orders=log(length(unique(Order_Num)))),by=Vendor])
    ggplot(to.plot,aes(x=n_orders)) + geom_histogram(binwidth=0.2) +
        xlab("log(# orders/vendor)")
})
output$h_org_subtype_per_type <- renderPlot({
    ts.data <- get.data()
    to.plot <- unique(ts.data[,list(n_subtype=length(unique(Org_Subtype))),by=Org_Type])
    ggplot(to.plot,aes(x=n_subtype)) + geom_histogram(binwidth=1) +
        xlab("# subtypes/org type")
})

output$gr_n_org_per_budget <- renderPlot({
    ts.data <- get.data()
    ts.data$budget_bins <- cut(ts.data$Budget,breaks=quantile(ts.data$Budget,probs=seq(0,1,by=0.1)),include.lowest=TRUE)
    to.plot <- unique(ts.data[,list(n_orgs=length(unique(Org))),by=budget_bins])

    ggplot(to.plot,aes(x=budget_bins,y=n_orgs)) + geom_point(size=4) +
        ylab("# orgs") +
    xlab("Budget") +
        geom_text(aes(x=budget_bins,y=n_orgs+10,label=as.character(n_orgs))) +
            theme(axis.text.x = element_text(angle=60,hjust=1))
})


})
