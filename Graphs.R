#####Created for Graph creation for Navon, Inverted Faces, Emoval + Demographics
###2019 Pilot VM Study
##Casey 08/14/2019
xyplot(logRT_mean~Session|bysubject, 
       data=Inv_stats_bysub_bydirection_bytime, 
       groups=Direction, 
       h=Inv_stats_bysub_bydirection_bytime$logRT_mean,
       auto.key =list(space="right", col=c("blue", "pink")), 
       main="Response Time by Subject", 
       ylab = "Response Time",
       scales=list(alternating=FALSE),
       panel = function(x, y, h, subscripts, groups) {
         panel.lmline(x, y, lty=3, lwd=1, col="purple")
         panel.abline(mean(h), lty=1, col="red")
         llines(x=x, y=y, type='p', pch=c(23, 23, 21, 21), col=c("blue", "blue", "pink", "pink"),
                fill=c("blue", "blue", "pink", "pink"))
         #ltext(x = x, y = y,labels = c("G", "L", "G", "L"), cex=1,
         #fontfamily = "HersheySans", col=c("blue", "pink", "blue", "pink"))
       },
       layout=c(5,1), aspect=5,
       axis=axis.grid
)

xyplot(logRT_mean~Session|Subject,
       data=Allresp_temp1,
       groups=TargetLocation,
       h=Allresp_temp1$logRT_mean,
       auto.key=list(space="right", col=c("blue", "pink")),
       main="Response Time by Subject", 
       ylab = "Response Time",
       scales=list(alternating=FALSE),
       panel = function(x, y, h, subscripts, groups) {
        #panel.lmline(x, y, lty=3, lwd=1, col="purple")
         #panel.abline(mean(h), lty=1, col="red")
         #llines(x=x, y=y, type='p', pch=c(23, 23, 21, 21), col=c("blue", "blue", "pink", "pink"),
                #fill=c("blue", "blue", "pink", "pink"))
          ltext(x = x, y = y,labels = groups, cex=1,
                fontfamily = "HersheySans", col=c("blue", "blue", "pink", "pink"))
       },
       layout=c(5,1), 
       aspect=5,
       axis=axis.grid,
)
       