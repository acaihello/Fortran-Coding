c     这个文档只是一个举例学习的作用，来源于钱诚老师主讲的《气候统计分析方法》提供的Fcode
      SUBROUTINE sort(n,arr)
      implicit none
      INTEGER n,M,NSTACK
      REAL arr(n)
      PARAMETER (M=7,NSTACK=50)
      INTEGER i,ir,j,jstack,k,l,istack(NSTACK)
      REAL a,temp
      jstack=0
      l=1
      ir=n    ! ir为数组的大小，假设其大于M，即执行else
1     if(ir-l.lt.M)then
        do 12 j=l+1,ir  !12 and 11 just a label of a circle
          a=arr(j)
          do 11 i=j-1,l,-1
            if(arr(i).le.a)goto 2   ! j-1到l向后扫描寻找小于当前值a
            arr(i+1)=arr(i)   !如果arr(i)>arr(j),从当前位置之后的整体向前移动一个单位,并将其赋给l
11        continue
          i=l-1
2         arr(i+1)=a
12      continue  !这就完成了6个数的排序，没有计算资源的浪费(goto 2直接跳过了循环)
        if(jstack.eq.0)return   ! 换区间来进行sort
        ir=istack(jstack)
        l=istack(jstack-1)
        jstack=jstack-2
      else
        k=(l+ir)/2  !这里取arr的中间的数(偶数个则取半)，+1为整型截断提供了保障
        temp=arr(k)
        arr(k)=arr(l+1)  !又把l+1和半值对调位置
        arr(l+1)=temp
        if(arr(l).gt.arr(ir))then    ! l大于尾，对调
          temp=arr(l)
          arr(l)=arr(ir)
          arr(ir)=temp
        endif
        if(arr(l+1).gt.arr(ir))then   ! l+1大于尾，对调
          temp=arr(l+1)
          arr(l+1)=arr(ir)
          arr(ir)=temp
        endif
        if(arr(l).gt.arr(l+1))then   ! l,l+1,tip顺序了
          temp=arr(l)
          arr(l)=arr(l+1)
          arr(l+1)=temp
        endif
        i=l+1    ! 从l=1开始浪费了一次循环，所以+1
        j=ir
        a=arr(l+1)
3       continue   ! continue是占位符，f77会使用，现在不用，使用go to 也会用到，不过go to 会让结构更混乱
          i=i+1
        if(arr(i).lt.a)goto 3  ! 如果arr(3)<arr(2),则比较arr(4)和arr(2),直到一个数大于arr(2)的arr(i)
4       continue
          j=j-1
        if(arr(j).gt.a)goto 4  ! 同样从尾部再来，大于arr(2)则向前找小于arr(2)的arr(j)
        if(j.lt.i)goto 5   ! 正向和反向相遇时
        temp=arr(i)
        arr(i)=arr(j)  ! 交换i,j
        arr(j)=temp
        goto 3   ! 这样会出现从2到ir之间存在一个点，前半段小于2，后半段大于2
5       arr(l+1)=arr(j)  !这个j就是临界点了
        arr(j)=a  !这里就是正确的一个点
        jstack=jstack+2
        if(jstack.gt.NSTACK)pause 'NSTACK too small in sort'  ! 暂停
        if(ir-i+1.ge.j-l)then  !如果大于arr(2)的个数多于小于的
          istack(jstack)=ir  ! 记录大于arr(2)的末尾数和大于arr(2)的起始数
          istack(jstack-1)=i
          ir=j-1  !末尾数改为小于arr(2)的个数
        else             ! 小于arr(2)的个数较多的话,记录小于arr(2)的末尾数以及小于arr(2)的起始数
          istack(jstack)=j-1  ! 通过改i来进行大于arr(2)的部分的处理，直到小的那一部分个数小于7
          istack(jstack-1)=l
          l=i
        endif   !大的一部分压入栈
      endif
      goto 1
      END


c      Ai给出的示意图如下：
c      SORT(arr, n)
c      │
c      ├── while (true)
c      │   ├── if (区间 < M)
c      │   │   ├── 插入排序
c      │   │   ├── 如果栈空 → 结束
c      │   │   └── 否则弹出下一区间
c      │   └── else
c      │       ├── 三数取中 → 选枢轴l+1
c      │       ├── 分区
c      │       ├── 压栈较大子区间
c      │       └── 处理较小子区间
c      └── end

