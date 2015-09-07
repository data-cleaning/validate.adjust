

context("adjust")


v <- validator(
  x + y == 10
  , x > 0
  , y > 0
)

dat <- data.frame(
    x = c(5,-5,5)
  , y = c(5, 1,2)
)

test_that("adjust happy flow",{
  
  # simplest case
  expect_equal(adjust(dat, v), data.frame(x=c(5,2,6.5),y=c(5,8,3.5)))
  expect_equal(adjust(data.frame(x=0,y=0), v), data.frame(x=5,y=5))

  
  # weights keeping ratio equal
  ad <- adjust(dat,v,weight="ratio")
  i <- c(1,3)
  expect_equal(ad$x[i]/ad$y[i], dat$x[i]/dat$y[i] ) 

  # equal ratio but with (numerically) zero record
  
  expect_equal(
    adjust(data.frame(x=0.0,y=1e-13),v,weight='ratio')
    ,data.frame(x=5,y=5)
  )
  
  # test single weights
  ad <- adjust(dat,v,weight=c(1,2)) 
  expect_equal((dat$x - ad$x),2*(dat$y-ad$y)) 
  
  # test weights per record.
  W <- matrix(c(
    1,1,
    1,1,
    1,2
  ),byrow=TRUE,nrow=3)
  expect_equal(adjust(dat,v,weight=W), data.frame(x=c(5,2,7),y=c(5,8,3)))
  
  # set adjustable variables
  ad <- matrix(TRUE,nrow=3,ncol=2)
  expect_equal(adjust(dat,v),adjust(dat,v,adjust=ad))
  ad[2,2] <- FALSE
  expect_equal(adjust(dat,v,adjust=ad),data.frame(x=c(5,9,6.5),y=c(5,1,3.5)))
  
})


test_that("adjust not-so happy flow",{
  expect_message(adjust(dat,validator(x>0,mean(x)>0)) )    
  expect_error(adjust(within(dat,x[1]<-"a"),v)) 
  expect_error(adjust(dat,validator(x>0),weight="foo"))
})
















