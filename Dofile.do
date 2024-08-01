log using "C:\Users\Usuario\OneDrive\Escritorio\3ro GANE\Mdo Trabajo\Informatica\Segundo Video\LogFile.smcl", replace

cd "C:\Users\Usuario\OneDrive\Escritorio\3ro GANE\Mdo Trabajo\Informatica\Segundo Video"
use "data.dta", clear

global h uhrswork1_1
global w hourwage_1

*Relación Muestral Total:
reg $h $w, robust
estimates store ols0
outreg2 ols0 using "C:\Users\Usuario\OneDrive\Escritorio\3ro GANE\Mdo Trabajo\Informatica\Segundo Video\Regresiones\ols0", replace //guardamos el resultado en la memoria local

////////////////////////////////////////////////////////////////////////////////

*Parte A: Analisis w y h

**1. Retoques previos
sum $h, detail
scalar hmax = 12*6
replace $h=. if $h>hmax | $h==0 

sum $w, detail
scalar miss = 999
replace $w=. if $w>=999

**2. Análisis Relación:
histogram $h, saving(horas.gph, replace)
histogram $w, saving(salario.gph, replace)

graph combine "horas.gph" "salario.gph", title("Valores Variables") saving(histograma_w+h.gph, replace)

***Estadísticos:
sum $h $w, detail
corr $h $w //Matriz varianzas-covarianzas

***Nube de puntos ante extistencia relación:
graph twoway scatter $h $w, title("Nube Puntos") saving(nube.gph, replace) 
graph twoway (lfit $h $w) (lowess $h $w), title("Relación") legend(label(1 "Lineal") label(2 "No lineal Suavizada")) saving(relacion.gph, replace) 

graph combine "nube.gph" "relacion.gph", title("Relación Variables") saving(Relación-Horas-Salario.gph, replace)

**3. Seleccionamos personas en edad de trabajar y empleada
keep if statefip_1==1| statefip_1==2 | statefip_1==4 | statefip_1==5

tab empstat_1
tab empstat_1, nolabel
keep if empstat_1==1 | empstat_1==10
keep if age_1<=65

**R*elación funcional y medición del efecto total:
reg $h $w, robust
estimates store ols1
outreg2 ols1 using "C:\Users\Usuario\OneDrive\Escritorio\3ro GANE\Mdo Trabajo\Informatica\Segundo Video\Regresiones\ols1", replace //guardamos el resultado en la memoria local

**4. Comparamos modelo bruto y submuestral concretado:
esttab ols0 ols1 using Table0_$muestras.rtf, se replace label varwidth(15) modelwidth(6)  cells(b(label(coef.) star fmt(%8.3f) ) se(label((Std. Err.)) par fmt(%6.2f))) starlevels(* 0.10 ** 0.05 *** 0.01) mtitles("Model 1" "Model 2") addnote("Comparamos los coeficientes estimados para muestra total y submuestra")

***Calculamos elasticidad para ver si la oferta de trabajo es elástica o no
qui reg $h $w, robust
margins, eyex($w) 

////////////////////////////////////////////////////////////////////////////////

*Parte B: Selección y ajuste variables Z 

**1. Variable Sexo
tab sex_1
tab sex_1, nolabel

replace sex_1=0 if sex_1==2
label define gen 1 "Hombre" 0 "Mujer"
label values sex_1 gen

***Comprobamos si es significativa: Regresion + Relación Lowess con variables h y w
graph twoway lowess $h sex_1, saving(horas_sexo.gph, replace) title("Relación Horas-Sexo")
graph twoway lowess $w sex_1, saving(salario_sexo.gph, replace) title("Relación Salario-Sexo")
graph combine horas_sexo.gph salario_sexo.gph, title("Relación Variables") saving(sex_1.gph, replace)

reg $h $w sex_1, robust
outreg2 sex_1 using "C:\Users\Usuario\OneDrive\Escritorio\3ro GANE\Mdo Trabajo\Informatica\Segundo Video\Regresiones\sex_1"

**2. Redefinimos educación
tab educ_1
tab educ_1, nolabel
rename educ_1 nivel_educ
replace nivel_educ=1 if nivel_educ<=20 & nivel_educ>0
replace nivel_educ=2 if nivel_educ<=71 & nivel_educ>20
replace nivel_educ=3 if nivel_educ<=73 & nivel_educ>71
replace nivel_educ=4 if nivel_educ<=81 & nivel_educ>73
replace nivel_educ=5 if nivel_educ<=92 & nivel_educ>81
replace nivel_educ=6 if nivel_educ<=111 & nivel_educ>92
replace nivel_educ=7 if nivel_educ<=125 & nivel_educ>111
label define exp 1 "Primaria" 2 "Primera Estapa Secundaria" 3 "Segunda Etapa Secundaria" 4 "Formación Profesional Media" 5 "FP Superior o Bachillerato" 6 "Grado Universitario" 7 "Master o Doctorado"
label values nivel_educ exp

gen educ=.
replace educ=0 if nivel_educ<=5
replace educ=1 if nivel_educ>=6
label define educ_lbl 0 "No Universitario" 1 "Universitario"
label values educ educ_lbl
 
***Significatividad
graph twoway lowess $h nivel_educ, saving(horas_educ.gph, replace) title("Relación Horas-Educación")
graph twoway lowess $w nivel_educ, saving(salario_educ.gph, replace) title("Relación Salario-Educación")
graph combine horas_educ.gph salario_educ.gph, title("Relación Variables") saving(educ.gph, replace)

reg $h $w educ, robust
outreg2 educ using "C:\Users\Usuario\OneDrive\Escritorio\3ro GANE\Mdo Trabajo\Informatica\Segundo Video\Regresiones\educ"

**3. Redefinimos Estado civil:
tab marst_1
tab marst_1, nolabel 

gen civil=.
replace civil=0 if marst_1>=3
replace civil=1 if  marst_1<=2
label define civil_lbl 0 "Sin Pareja" 1 "Con Pareja"
label values civil civivil_lbl

***Significatividad:
graph twoway lowess $h civil, saving(horas_civil.gph, replace) title("Relación Horas-Estado Civil")
graph twoway lowess $w civil, saving(salario_civil.gph, replace) title("Relación Salario-Estado Civil")
graph combine horas_civil.gph salario_civil.gph, title("Relación Variables") saving(civil.gph, replace)

reg $h $w civil, robust
outreg2 civil using "C:\Users\Usuario\OneDrive\Escritorio\3ro GANE\Mdo Trabajo\Informatica\Segundo Video\Regresiones\civil"
//No significativo

**4. Redefinimos hijos:
tab nchild_1

gen hijos=0 if nchild_1!=. | nchild_1==0
replace hijos=1 if nchild_1>=1
label define hijos_lbl 0 "Sin hijos" 1 "Con Hijos"
label values hijos hijos_lbl

***Significatividad:
graph twoway lowess $h hijos, saving(horas_hijos.gph, replace) title("Relación Horas-Hijos")
graph twoway lowess $w hijos, saving(salario_hijos.gph, replace) title("Relación Salario-Hijos")
graph combine horas_hijos.gph salario_hijos.gph, title("Relación Variables") saving(hijos.gph, replace)

reg $h $w hijos, robust
outreg2 hijos using "C:\Users\Usuario\OneDrive\Escritorio\3ro GANE\Mdo Trabajo\Informatica\Segundo Video\Regresiones\hijos"

**5. Redefinimos tipo de empleo:
tab classwkr_1
tab classwkr_1, nolabel

gen empleo=.
replace empleo=0 if classwkr_1==25 | classwkr_1==26 | classwkr_1==27 | classwkr_1==28
replace empleo=1 if classwkr_1==13 | classwkr_1==14
replace empleo=2 if classwkr_1==21
label define empleo_lbl 0 "Estado" 1 "Autoempleado" 2 "Empleado"
label values empleo empleo_lbl

tab empleo

gen us=0 if empleo==1 |empleo==2
replace us=1 if empleo==0

gen aut=0 if empleo==0 |empleo==2
replace aut=1 if empleo==1

gen priv=0 if empleo==0 |empleo==1
replace priv=1 if empleo==2

***Significatividad:
graph twoway lowess $h empleo, saving(horas_empleo.gph, replace) title("Relación Horas-Empleo")
graph twoway lowess $w empleo, saving(salario_empleo.gph, replace) title("Relación Salario-Empleo")
graph combine horas_empleo.gph salario_empleo.gph, title("Relación Variables") saving(empleo.gph, replace)

reg $h $w aut priv, robust
outreg2 empleo using "C:\Users\Usuario\OneDrive\Escritorio\3ro GANE\Mdo Trabajo\Informatica\Segundo Video\Regresiones\empleo" //No existe relación 

**6. Redifinimos edad:
tab age_1

gen edad=.
replace edad=0 if age_1>=25 & age_1<35
replace edad=1 if age_1>=35 & age_1<50
replace edad=2 if age_1>=50 & age_1<=65
label define edad_lbl 0 "Jóvenes" 1 "Adultos" 2 "Mayores"
label values edad edad_lbl
tab edad

gen jov=0 if edad==1 | edad==2
replace jov=1 if edad==0
tab jov

gen adul=0 if edad==0 | edad==2
replace adul=1 if edad==1
tab adul

gen may=0 if edad==0 | edad==1
replace may=1 if edad==2
tab may

***Significatividad:
graph twoway lowess $h edad, saving(horas_edad.gph, replace) title("Relación Horas-Edad")
graph twoway lowess $w edad, saving(salario_edad.gph, replace) title("Relación Salario-Edad")
graph combine horas_edad.gph salario_edad.gph, title("Relación Variables") saving(edad.gph, replace)

reg $h $w may adul, robust
outreg2 empleo using "C:\Users\Usuario\OneDrive\Escritorio\3ro GANE\Mdo Trabajo\Informatica\Segundo Video\Regresiones\edad"
//No significativa

**7. Redifinimos zona:
tab region_1
tab region_1, nolabel

gen zona=.
replace zona=0 if region_1==33
replace zona=1 if region_1==32
label define zona_lbl 0 "Costa Oeste" 1 "Costa Este"
label values zona zona_lbl

***Significatividad:
graph twoway lowess $h zona, saving(horas_zona.gph, replace) title("Relación Horas-Zona")
graph twoway lowess $w zona, saving(salario_zona.gph, replace) title("Relación Salario-Zona")
graph combine horas_zona.gph salario_zona.gph, title("Relación Variables") saving(zona.gph, replace)

reg $h $w zona, robust
outreg2 zona using "C:\Users\Usuario\OneDrive\Escritorio\3ro GANE\Mdo Trabajo\Informatica\Segundo Video\Regresiones\zona" 
//Variable no significativa

**8. Redifinimos raza:
tab race_1
tab race_1, nolabel

gen raza=.
replace raza=0 if race_1==200 | race_1==651
replace raza=1 if race_1==100
label define raza_lbl 0 "Negra o Asiática" 1 "Blanca"
label values raza raza_lbl
tab raza

***Significatividad:
graph twoway lowess $h raza, saving(horas_raza.gph, replace) title("Relación Horas-Raza")
graph twoway lowess $w raza, saving(salario_raza.gph, replace) title("Relación Salario-Raza")
graph combine horas_raza.gph salario_raza.gph, title("Relación Variables") saving(raza.gph, replace)

reg $h $w raza, robust
outreg2 raza using "C:\Users\Usuario\OneDrive\Escritorio\3ro GANE\Mdo Trabajo\Informatica\Segundo Video\Regresiones\raza" 

**9. Definimos variables z:
global z sex_1 educ hijos raza
sum $z

**10. Estimamos primer modelo lineal: ET=B1
reg $h $w $z, robust
estimates store ols2
outreg2 ols2 using "C:\Users\Usuario\OneDrive\Escritorio\3ro GANE\Mdo Trabajo\Informatica\Segundo Video\Regresiones\ols2"

**11. Estimamos primer modelo cuadrático ET=B1+B2
reg $h $w c.$w#c.$w $z, robust
estimates store ols3
outreg2 ols3 using "C:\Users\Usuario\OneDrive\Escritorio\3ro GANE\Mdo Trabajo\Informatica\Segundo Video\Regresiones\ols3"

**12. Comparamos regresiones:
esttab ols2 ols3 using Table1_$muestras.rtf, se replace label varwidth(15) modelwidth(6)  cells(b(label(coef.) star fmt(%8.3f) ) se(label((Std. Err.)) par fmt(%6.2f))) starlevels(* 0.10 ** 0.05 *** 0.01) mtitles("Model 1" "Model 2") addnote("Comparamos los coeficientes estimados para modelo lineal y cuadrático")

**13. Calculamos Efecto Marginal Total y Elasticidad h frente w
margins, dydx($w) 
//Podemos observar como el efecto marginal total coincide con el efecto marginal del salario del modelo lineal
margins, eyex($w) 

**14. Hallamos turning point: h en el que ES=ER
nlcom _b[$w]/(-2*_b[c.$w#c.$w])
histogram $w, bin(50) xline(42)

////////////////////////////////////////////////////////////////////////////////

*PARTE C y D: Selección y ajuste variables Valores

**1. Creación variable Renta no laboral:
gen ing_sem=hhincome_1/52 //Pasamos de ingresos anuales a semanales
gen wh=$w*$h //Calculamos ingreso laboral semanal
gen v =ing_sem-wh //creamos variable RNL
gen peso=v/ing_sem //Peso rnl en ingreso laboral semanal
replace peso=0 if peso<0 //Eliminamos valores negativos
histogram peso, bin(50) title("peso") saving(peso,replace)
tabstat  peso, stats(p5 p25 mean p75 p95) //Análisis individuos con rnl 

***Corregimos valores negativos y creamos dummy rnl: 
replace v=0 if (v/ing_sem)<=0.01 //Eliminamos valores negativos

gen byte otroing=0 
replace otroing=1 if v >0 & v <. //Variable para ver si individuos tienen rnl

***Observamos resultado y corregimos valores extremos de v (mayores al percentil 99):
sum v, detail
sum v if v>=0, detail
return list //Para ver unión de resultados anteriores
drop if v>r(p99)

**2. Analizamos Significatividad V: Modelo Lineal (ER)
histogram v, bin(50) saving(v.gph,replace) title("Renta No Laboral (V)")
graph twoway (lpoly $h v) (lowess $h v) (lfit $h v) ,  saving(h-v.gph,replace) legend(label(2 "No Lineal Suavizada") label(3 "Tendencia Lineal") label(1 "Tendencia No Lineal")) title("Relación H-V") xtitle("Horas Trabajadas")
graph twoway (lpoly $w v) (lowess $w v) (lfit $w v)  , saving(w-v.gph,replace)  legend(label(2 "No Lineal Suavizada") label(3 "Tendencia Lineal") label(1 "Tendencia No Lineal"))  title("Relación W-V") xtitle("Salario Hora")
graph combine h-v.gph w-v.gph v.gph, title("Scatter Plot `v' `$w' `$h'",size(small)) 

reg $h $w c.$w#c.$w $z v, robust
estimates store ols4
outreg2 ols4 using "C:\Users\Usuario\OneDrive\Escritorio\3ro GANE\Mdo Trabajo\Informatica\Segundo Video\Regresiones\ols4"

**3. Relación No Lineal
reg $h $w c.$w#c.$w $z v c.v#c.v, robust
estimates store ols5
outreg2 ols5 using "C:\Users\Usuario\OneDrive\Escritorio\3ro GANE\Mdo Trabajo\Informatica\Segundo Video\Regresiones\ols5"

**4. Comparamos regresiones:
esttab ols4 ols5 using Table2_$muestras.rtf, se replace label varwidth(15) modelwidth(6)  cells(b(label(coef.) star fmt(%8.3f) ) se(label((Std. Err.)) par fmt(%6.2f))) starlevels(* 0.10 ** 0.05 *** 0.01) mtitles("Model 1" "Model 2") addnote("Comparamos los coeficientes estimados para modelo lineal y cuadrático")

**5. Calculamos Efecto Marginal Total y Elasticidad h frente w
margins, dydx(v) 
//Podemos observar como el efecto renta coincide con el efecto marginal de v del modelo lineal
margins, eyex(v) 

**6. Hallamos turning point: v en el que ER=0
nlcom _b[v]/(-2*_b[c.v#c.v])
histogram v, bin(50) xline(9992.984)

**7. Representación oferta laboral: Comprobamos lo visto en teoría (Labour Supply Curve)
qui reg $h $w c.$w#c.$w $z v, robust
capture drop p$h
predict p$h

preserve 
collapse  p$h, by($w)
graph twoway (scatter $w p$h), title("Oferta Laboral") saving(hs.gph,replace)
restore 
graph use hs.gph

**8. Representación oferta laboral: ¿Cambio por sexo?
preserve
collapse  p$h, by($w sex_1)
graph twoway (scatter $w p$h) if sex_1==1 ,title("Hombre") saving(hom.gph,replace)
graph twoway (scatter $w p$h) if sex_1==0 ,title("Mujer") saving(muj.gph,replace)
graph combine hom.gph muj.gph
restore

**9. Elasticidades oferta de trabajo por género:
gen lgh= log10(uhrswork1_1)
gen lgw=log10(hourwage_1)
gen lgv=log10(v)

***Hombres
reg lgh lgw $z lgv if sex_1==1, robust 

***Mujeres
reg lgh lgw $z lgv if sex_1==0, robust 

**10. Oferta de Trabjo por nivel educativo:
preserve
collapse  p$h, by($w educ)
graph twoway (scatter $w p$h) if educ==1 ,title("Universitario o Superior") saving(hseduc1.gph,replace)
graph twoway (scatter $w p$h) if educ==0 ,title("No Universitario") saving(hseduc0.gph,replace)
graph combine hseduc1.gph hseduc0.gph
restore

log close

