      program path

c     variables
      character filename*100
      integer fh
      byte c
      integer height
      integer width
      integer x,y,d
      integer inp(10000)
      integer dist(10000)
      integer stx(10000)
      integer sty(10000)
      integer std(10000)
      integer sttop
      
c     initialization
      width=0
      height=0
      sttop=1
      
      do 200 x=1,10000
         inp(x)=0
         dist(x)=-1
 200  continue
      
c     read inp file
      x=1
      height=1
      call GETARG(1,filename)
      open (1,FILE=filename,STATUS='OLD')
 10   read (1,100,EOR=20,END=30,ADVANCE='no') c
      inp(x) = c-48
      x=x+1
      go to 10
 20   if (height .EQ. 1) width=x-1
      height=height+1
      go to 10
 30   height=height-1
      close (fh)

      call stackpush(stx,sty,std,sttop,1,1,0)
      
 205  if (sttop .EQ. 1) go to 206
      call stackpop(stx,sty,std,sttop,x,y,d)
      call updatedist(stx,sty,std,sttop,dist,inp,width,height,x-1,y,d)
      call updatedist(stx,sty,std,sttop,dist,inp,width,height,x+1,y,d)      
      call updatedist(stx,sty,std,sttop,dist,inp,width,height,x,y-1,d)
      call updatedist(stx,sty,std,sttop,dist,inp,width,height,x,y+1,d)
      go to 205
 206  continue
      write (*,*) dist(width*height)
      stop

c     single character format
 100  format(A1)
      end
      
c     operational stack
      subroutine swapint(a,b)
      integer a,b,tmp
      tmp=a
      a=b
      b=tmp
      return
      end
      
      subroutine stackpush (stx,sty,std,sttop,x,y,d)
      integer x,y,d,sttop
      integer stx(10000)
      integer sty(10000)
      integer std(10000)
      integer i,currentd,currentx,currenty
      currentd=d
      currentx=x
      currenty=y
      do 202 i=1,sttop-1
         if (std(i) .LE. currentd) then
            call swapint(std(i),currentd)
            call swapint(stx(i),currentx)
            call swapint(sty(i),currenty)
         endif
 202  enddo
      stx(sttop)=currentx
      sty(sttop)=currenty
      std(sttop)=currentd
      sttop=sttop+1
      return
      end

      subroutine stackpop (stx,sty,std,sttop,x,y,d)
      integer x,y,d,sttop
      integer stx(10000)
      integer sty(10000)
      integer std(10000)
      sttop=sttop-1
      x=stx(sttop)
      y=sty(sttop)
      d=std(sttop)
      return
      end
      
      subroutine updatedist (stx,sty,std,sttop,dist,inp,w,h,x,y,d)
      integer sttop,w,h,x,y,d,nd
      integer stx(10000)
      integer sty(10000)
      integer std(10000)
      integer dist(10000)
      integer inp(10000)
      if (dist(x+w*(y-1)) .GE. 0) go to 210
      if (x .LE. 0) go to 210
      if (y .LE. 0) go to 210
      if (x .GE. w+1) go to 210
      if (y .GE. h+1) go to 210
      nd=inp(x+w*(y-1))+d
      dist(x+w*(y-1))=nd
      call stackpush(stx,sty,std,sttop,x,y,nd)
 210  return
      end
