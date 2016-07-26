# Evenwel-vs.-Abbot-Case
Gerrymanderring Analysis
library ( l a w s t a t )		



#Read CSV F i l e

vap=read . csv ( ’~/Dropbox/ f i l e s /Legal  Stat / case	a n a l y s i s /Evenwel  case /data/VAPd



#Convert  d a t a	i n t o  numeric  d a t a

for ( i  in  2:7){

vap [ , i ]=as . numeric( gsub( " , " ,  "" , vap [ , i ] ) )

}

for ( i  in  8:12){

vap [ , i ]=as . numeric( gsub( "%" ,  "" , vap [ , i ] ) )

}

attach ( vap )



#Function  f o r	T o t a l  pop ,	i d e a l  pop  and	d i s t r i c t  number

basic_stat=function ( data ){

n=nrow( data )

totalsum=sum( data )

idealpop=totalsum /n



out1=rbind ( TotalStatePopulation=totalsum , TotalDistrictsRequired=n , I d e a l D i s t r
 


 


return ( round( out1 , d i g i t s = 0 ) )

}



#Ba s i c  C h a r a c t e r s

b a s i c_stat ( Voting . Age . P o p u l a t i o n )



#Function f o r Plan o v e r a l l range , s m a l l e s t , l a r g e s t pop b a s i c_range=function ( data ) {

s m a l l=min( data ) smallnum=which . min( data ) l a r g e=max( data ) largenum=which .max( data ) r a n g e o v e r=l a r g e s m a l l

ave=mean( data )

out2=rbind ( PlanOverallRange=r a n g e o v e r , S m a l l e s t D i s t r i c t N u m=smallnum ,

S m a l l e s t D i s t r i c t=s m a l l , L a r g e s t D i s t r i c t N u m=largenum , L a r g e s t D i s t r i c Average=ave )

return ( round( out2 , d i g i t s = 0 ) )

}



#Ba s i c  Range

b a s i c_range ( Voting . Age . P o p u l a t i o n )



#Function f o r p e r c e n t a g e d e v i a t i o n o f p l a n o v e r a l l range , s m a l l e s t , l a r g e s t pop d e v i a t i o n=function ( data ) {
 

 

	s m a l l=length ( data )		
	l a r g e=max( data )		
	ave=mean( data )		
	por=l a r g e  s m a l l		
	ppor=por/ave		
	sdd=abs ( s m a l l  ave )		
	psdd=sdd/ave		
	l d d=l a r g e  ave		
	pldd=l d d /ave		
	z1=rbind ( por , sdd , l d d )		
	z2=rbind ( ppor , psdd , pldd )
	out3=cbind ( round( z1 , d i g i t s = 0 ) , z2 )
	colnames ( out3 )=c ( ’ Total	Absolute  Deviation ’ , ’ Percent  Deviation ’ )
	rownames( out3)=c ( ’ Plan	Overall  Rnage ’ , ’ Smallest  D i s t r i c t ’ , ’ Largest  D i s t r i c ’ )
	return ( out3 )		
}		
#D e v i a t i o n		
	deviation ( Voting . Age . Population )



#Lorenz	Curve					
l o r e n z . curve ( Voting . Age . Population , seq ( from=1, to =1) ,
main =	" Lorenz  Curve  f o r  Voting  Age  Population " ,
xlab	=	"Cumlative	Fraction	of	the	Texas	L e g i s l a t u r e " ,
ylab	=	"Cumlative	Fraction	of	the	Voting	Age  Population " )
 





 


#Gini  I n d e x

g i n i . index ( Voting . Age . P o p u l a t i o n )





#Function f o r c a l c u l a t i n g d i f f e r e n c e , d e v i a t i o n #and v o t i n g power s h a r e f o r each d i s t r i c t


d	i s t r i c t_stat=function ( data ) { n=length ( data )

t o t a l s u m=sum( data )

i d e a l p o p=round( t o t a l s u m /n , d i g i t s = 0 ) l o w d i s t r i c t=rep (min( data ) , n ) ad=data i d e a l p o p

d	f i=abs ( data i d e a l p o p ) / i d e a l p o p vtp=sum( 1 /data )

v t p s =(1/data ) /vtp



d	i s t=t ( t ( 1 : 3 1 ) )

out4=as . data . frame( cbind ( d i s t , Voting . Age . P op ul a ti on , ad , d f i , v t p s ) ) colnames ( out4 )=c ( ’ D i s t r i c t ’ , ’VAP ’ , ’ D i f f e r e n c e ’ , ’ D e v i a t i o n ’ , ’ VotingPower ’ ) return ( out4 )

}



# S t a t i s t i c s	f o r  Each  D i s t r i c t

d i s t r i c t_stat ( Voting . Age . P o p u l a t i o n )
 


 





#Wilcoxon  Test

anglopop=X. Anglo [X. Anglo > X. B l a c k H i s p a n i c ] anglopop

minpop=X. B l a c k H i s p a n i c [X. BlackHispanicH > X. Anglo ] minpop

summary( anglopop ) summary( minpop )

#For  d a t a  w i t h o u t	t i e s

w i l c o x . text ( minpop , anglopop )



#For d a t a w i t h t i e s library ( exactRankTests )

w i l c o x . e x a c t ( minpop , anglopop )



#B o x p l o t

boxplot ( minpop , anglopop , names=c ( ’ Minority Population ’ , ’ Anglo Population ’ ) , main = " Minority and Anglo Population Percentage " , ylab=’ Percentage%’ )
 


A.2	Data Set

A.2.1	Raw Data Set

Data Source: PlanS172_Red202(ftp://ftpgis1.tlc.state.tx.us/PlanS172/Reports/PDF/)


Variable Discription:

District: District Number

Voting Age Population: Voting Age Population(18+) in each district

Anglo: Anglo voting age population

Black: Black voting age population

Hispanic: Hispanic voting age population

BlackHispanic: sum of Black and Hispanic voting age population

Other: Other racial/ethnic population

%(Anglo,Black,Hispanic,BlackHispanic,Other) : Percentage of total voting age population
 



