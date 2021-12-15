      program path

c     variables
      character filename*100
      byte c
      integer fh
      integer*8 height
      integer*8 width
      integer*8 x,y,d
      integer*8 inp(10000)
      integer*8 dist(2500000)
      integer*8 stx(2500000)
      integer*8 sty(2500000)
      integer*8 std(2500000)
      integer*8 sttop
      integer*8 zero,one
      
c     initialization
      width=0
      height=0
      sttop=1
      zero=0
      one=1
      
      do 200 x=1,10000
         inp(x)=0
 200  continue
      do 201 x=1,250000
         dist(x)=-1
 201  continue
      
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

      call stackpush(stx,sty,std,sttop,one,one,zero)
      
 205  if (sttop .EQ. 1) go to 206
      call stackpop(stx,sty,std,sttop,x,y,d)
      call updatedist(stx,sty,std,sttop,dist,inp,width,height,x-1,y,d)
      call updatedist(stx,sty,std,sttop,dist,inp,width,height,x+1,y,d)      
      call updatedist(stx,sty,std,sttop,dist,inp,width,height,x,y-1,d)
      call updatedist(stx,sty,std,sttop,dist,inp,width,height,x,y+1,d)
      go to 205
 206  continue
      write (*,*) dist(width*5*height*5)
      stop

c     single character format
 100  format(A1)
      end
      
c     operational stack
      subroutine swapint(a,b)
      integer*8 a,b,tmp
      tmp=a
      a=b
      b=tmp
      return
      end
      
      subroutine stackpush (stx,sty,std,sttop,x,y,d)
      integer*8 x,y,d,sttop
      integer*8 stx(250000)
      integer*8 sty(250000)
      integer*8 std(250000)
      integer*8 i,currentd,currentx,currenty
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
      integer*8 x,y,d,sttop
      integer*8 stx(250000)
      integer*8 sty(250000)
      integer*8 std(250000)
      sttop=sttop-1
      x=stx(sttop)
      y=sty(sttop)
      d=std(sttop)
      return
      end
      
      subroutine updatedist (stx,sty,std,sttop,dist,inp,w,h,x,y,d)
      integer*8 sttop,w,h,x,y,d,nd
      integer*8 stx(250000)
      integer*8 sty(250000)
      integer*8 std(250000)
      integer*8 dist(250000)
      integer*8 inp(10000)
      if (dist(x+(w*5)*(y-1)) .GE. 0) go to 210
      if (x .LE. 0) go to 210
      if (y .LE. 0) go to 210
      if (x .GE. (w*5)+1) go to 210
      if (y .GE. (h*5)+1) go to 210
      call lookupdist(inp,w,h,x,y,nd)
      nd=nd+d
      dist(x+(w*5)*(y-1))=nd
      call stackpush(stx,sty,std,sttop,x,y,nd)
 210  return
      end
      
      subroutine lookupdist(inp,w,h,x,y,d)
      integer*8 w,h,x,y,d
      integer*8 inp(10000)
      integer*8 xmod,ymod,xdiv,ydiv
      xmod=mod(x-1,w)+1
      xdiv=(x-1) / w
      ymod=mod(y-1,h)+1
      ydiv=(y-1) / h      
      d=inp(xmod+w*(ymod-1))+xdiv+ydiv
      d=mod(d-1,9)+1
      return
      end
