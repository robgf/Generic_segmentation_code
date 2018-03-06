polyline = data.frame(
  x = c(-5,1,5,7,8,12,14,16,17,13), # x
  y = c(0,11,3,8,2,15,9,13,15,23)   # y
)

plot(polyline$x, polyline$y, type="l", asp=1, lwd=1)
points(polyline$x, polyline$y, pch=4, cex=4, col="gray")

polyline2 = resample_polyline(polyline, interval_length = 5, add_final_point = FALSE, add_original_points = TRUE)
lines(polyline2$x,  polyline2$y, col="red", lty=4, lwd=3)
points(polyline2$x, polyline2$y, pch=19)

legend("topleft",
       c("original points", "added points", "resulting line"),
       pch = c(4, 19, NA),
       lty = c(NA, NA, 2),
       pt.cex = c(4,1,1),
       col = c("gray", "black", "red")
)
