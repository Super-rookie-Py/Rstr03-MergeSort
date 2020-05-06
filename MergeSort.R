### 2020/05/06 Keonwoo Park

### 데이터 구조론
## Merge Sort


Merge_Sort_I <- function(d){
  size_d = length(d)
  if(size_d > 2){
    # Decompose
    size_first_half= floor(size_d/2) # 내림
    size_second_half = size_d-size_first_half
    
    d1=Merge_Sort_I(d[1:size_first_half])
    d2=Merge_Sort_I(d[(size_first_half+1):size_d])
    
    # Merge
    i1=1
    i2=1
    i=1
    repeat{
      if(d1[i1]<d2[i2]){
        d[i]=d1[i1]
        i1 = i1+1
      }else{
        d[i]=d2[i2]
        i2=i2+1
      }
      i=i+1
      
      if(i1>size_first_half){
        d[i:size_d]=d2[i2:size_second_half]
        return(d)
      }else if(i2>size_second_half){
        d[i:size_d]=d1[i1:size_first_half]
        return(d)
      }
    }
  }else if(size_d == 2){
    if(d[1]>d[2]){
      return(c(d[2],d[1])) # 두 개를 바꾸고 묶어서 반환 
    }else{
      return(d)
    }
  }else{
    return(d) #크기가 1이면 그냥 반환 
  }
}
Merge_Sort_D <- function(d){
  size_d = length(d)
  if(size_d > 2){
    # Decompose
    size_first_half= floor(size_d/2) # 내림
    size_second_half = size_d-size_first_half
    
    d1=Merge_Sort_D(d[1:size_first_half])
    d2=Merge_Sort_D(d[(size_first_half+1):size_d])
    
    # Merge
    i1=1
    i2=1
    i=1
    repeat{
      if(d1[i1]>d2[i2]){
        d[i]=d1[i1]
        i1 = i1+1
      }else{
        d[i]=d2[i2]
        i2=i2+1
      }
      i=i+1
      
      if(i1>size_first_half){
        d[i:size_d]=d2[i2:size_second_half]
        return(d)
      }else if(i2>size_second_half){
        d[i:size_d]=d1[i1:size_first_half]
        return(d)
      }
    }
  }else if(size_d == 2){
    if(d[1]<d[2]){
      return(c(d[2],d[1])) # 두 개를 바꾸고 묶어서 반환 
    }else{
      return(d)
    }
  }else{
    return(d) #크기가 1이면 그냥 반환 
  }
  
}

Merge_sort <- function(d, decreasing=FALSE){
  if(decreasing==F){
    Merge_Sort_I(d)
  }else{
    Merge_Sort_D(d)
  }
  
}


# 알고리즘에서 if문과 for문을 사용하면 속도에 영향을 미친다.
# 함수를 선언하는것도 속도에 영향을 주긴 하지만 속도 보다는 함수를 
# 선언 할 때마다 메모리에 영향을 주는 점이 크다.
# 그렇지만 프로그래밍을 할 때는 역할을 여러개의 함수로 나눠서
# 프로그래밍을 하는 것이 좋다.




a <- sample(1:20)
a
Merge_sort(a,T)




#알고리즘 속도측정
set.seed(1234)
test_data<-data.frame(d1=sample(1:1000),
                      d2=1:1000,
                      d3=1000:1,
                      d4=c(1:500,sample(501:1000)),
                      d5=c(sample(1:500),c(501:1000))
                      )
head(test_data)



n=5
Simulation_Results <- data.frame(d1=rep(0,n),
                                 d2=rep(0,n),
                                 d3=rep(0,n),
                                 d4=rep(0,n),
                                 d5=rep(0,n))




for(i3 in 1:4){
  for (i1 in 1:n){
    for (i2 in 1:5){
      T1<-Sys.time()
      if(i3==1){
        Selection_Sort(test_data[,i2])
      }else if(i3==2){
        Bubble_Sort(test_data[,i2])
      }else if(i3 == 3){
        Insertion_Sort(test_data[,i2])
      }else{
        Merge_sort(test_data[,i2])
      }
      T2<-Sys.time()
      T3=T2-T1
      Simulation_Results[i1,i2]=as.numeric(difftime(T2,T1,units="secs"))

    }
  }
  print(Simulation_Results)
  print(apply(Simulation_Results,2,mean))
}
abc<-test_data[,1]
abc
Merge_sort(abc)
