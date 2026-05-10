load_all()

fssg(survival::Surv(time, status) ~ x, data = survival::aml, skip='NA') -> output1


idk <- data.frame(dist='1', direction='NA')[-1,]
iter <- 1
for(i in output1$models){
  tempname = names(output1$models)[iter]
  print(tempname)
  val <- pull(last(select(as.data.frame(i$res.t),est)))
  direct <- ifelse( val < 0, 'AFT', 'PH')
  print(paste('val =', val,'so prob', direct))
  plot(
    i,
    newdata = data.frame(x=c('Maintained','Nonmaintained')), col=c('blue','red'),
    type='survival',
    lwd=2,
    main=paste(tempname)
  )
  legend(
    'topright',
    legend = c('M','Not-M'),
    col=c('blue','red'),
    lwd=2
  )
  idk %>% add_row(
    dist = tempname,
    direction = direct
  ) -> idk
  iter = iter+1
}





fssg(survival::Surv(time, status) ~ as.factor(sex), data = survival::cancer, skip='NA') -> output2
idk <- data.frame(dist='1', direction='NA')[-1,]
iter <- 1
for(i in output2$models){
  tempname = names(output2$models)[iter]
  print(tempname)
  val <- pull(last(select(as.data.frame(i$res.t),est)))
  direct <- ifelse( val > 0, 'AFT', 'PH')
  print(paste('val =', val,'so prob', direct))
  plot(
    i,
    newdata = data.frame(sex=c('1','2')), col=c('blue','red'),
    type='survival',
    lwd=2,
    main=paste(tempname)
  )
  legend(
    'topright',
    legend = c('M','Not-M'),
    col=c('blue','red'),
    lwd=2
  )
  idk %>% add_row(
    dist = tempname,
    direction = direct
  ) -> idk
  iter = iter+1
}

