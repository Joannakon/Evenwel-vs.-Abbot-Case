# Evenwel-vs.-Abbot-Case
Gerrymanderring Analysis
library ( lawstat )		



#Read CSV File

vap=read . csv ( ’~/Dropbox/ f i l e s /Legal  Stat / case analysis /Evenwel  case /data/VAPd



#Convert  data	into  numeric  data

for ( i  in  2:7){

vap [ , i ]=as . numeric( gsub( " , " ,  "" , vap [ , i ] ) )

}

for ( i  in  8:12){

vap [ , i ]=as . numeric( gsub( "%" ,  "" , vap [ , i ] ) )

}

attach ( vap )



#Function  for	Total  pop ,	ideal  pop  and	district  number

basic_stat=function ( data ){

n=nrow( data )

totalsum=sum( data )

idealpop=totalsum /n



out1=rbind ( TotalStatePopulation=totalsum , TotalDistrictsRequired=n , Ideal Distr
 


 


return ( round( out1 , digits = 0 ) )

}



#Basic  Characters

basic_stat ( Voting . Age . Population )



#Function for Plan overall range , smallest , largest pop basic_range=function ( data ) {

small=min( data ) smallnum=which . min( data ) large=max( data ) largenum=which .max( data ) rangeover=largesmall

ave=mean( data )

out2=rbind ( PlanOverallRange=rangeover , Smallest District Num=smallnum ,

Smallest District=small , Largest District Num=largenum , Largest Distric Average=ave )

return ( round( out2 , digits = 0 ) )

}



#Basic  Range

basic_range ( Voting . Age . Population )



#Function for percentage deviation of plan overall range , smallest , largest pop deviation=function ( data ) {
 

 

	small=length ( data )		
	large=max( data )		
	ave=mean( data )		
	por=large  small		
	ppor=por/ave		
	sdd=abs ( small  ave )		
	psdd=sdd/ave		
	ldd=large  ave		
	pldd=ldd /ave		
	z1=rbind ( por , sdd , ldd )		
	z2=rbind ( ppor , psdd , pldd )
	out3=cbind ( round( z1 , digits = 0 ) , z2 )
	colnames ( out3 )=c ( ’ Total	Absolute  Deviation ’ , ’ Percent  Deviation ’ )
	rownames( out3)=c ( ’ Plan	Overall  Rnage ’ , ’ Smallest  District ’ , ’ Largest  Distric ’ )
	return ( out3 )		
}		
#Deviation		
	deviation ( Voting . Age . Population )



#Lorenz	Curve					
lorenz . curve ( Voting . Age . Population , seq ( from=1, to =1) ,
main =	" Lorenz  Curve  f o r  Voting  Age  Population " ,
xlab	=	"Cumlative	Fraction	of	the	Texas	Legislature " ,
ylab	=	"Cumlative	Fraction	of	the	Voting	Age  Population " )
 





 


#Gini  I n d e x

gini . index ( Voting . Age . Population )





#Function for calculating difference , deviation #and voting power share for each district


district_stat=function ( data ) { n=length ( data )

totalsum=sum( data )

idealpop=round( totalsum /n , digits = 0 ) low district=rep (min( data ) , n ) ad=data idealpop

dfi=abs ( data idealpop ) / idealpop vtp=sum( 1 /data )

v t p s =(1/data ) /vtp



dist=t ( t ( 1 : 3 1 ) )

out4=as . data . frame( cbind ( dist , Voting . Age . Population , ad , dfi , vtps ) ) colnames ( out4 )=c ( ’ District ’ , ’VAP ’ , ’ Difference ’ , ’ Deviation ’ , ’ VotingPower ’ ) return ( out4 )

}



# Statistics	for  Each  District

district_stat ( Voting . Age . Population )
 


 





#Wilcoxon  Test

anglopop=X. Anglo [X. Anglo > X. Black Hispanic ] anglopop

minpop=X. B l a c k H i s p a n i c [X. BlackHispanicH > X. Anglo ] minpop

summary( anglopop ) summary( minpop )

#For  data  without	ties

wilcox . text ( minpop , anglopop )



#For data with ties 
library ( exactRankTests )

wilcox. exact ( minpop , anglopop )



#Boxplot

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
 


