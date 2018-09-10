module AreasVols ( Area(Esfera, Cilindro, Cone, TroncoDeCone, EsferoideOblato, EsferoideProlato), 
				Volume(Esfera, Cilindro, Cone, TroncoDeCone, EsferoideOblato, EsferoideProlato),
				 Raio, Alt, Raioalt, Raiobase, A, B, Ex, areaLateral, areaTotal, vol) where

data Area = Esfera Raio
		  | Cilindro Raio Alt
		  | Cone Raio Alt 
		  | TroncoDeCone Raiobase Raioalt Alt
		  | EsferoideOblato A B Ex 
		  | EsferoideProlato A B Ex
			deriving Show
			
data Volume = Esfera Raio
			| Cilindro Raio Alt
			| Cone Raio Alt 
			| TroncoDeCone Raiobase Raioalt Alt
			| EsferoideOblato A B Ex 
			| EsferoideProlato A B Ex
			deriving Show

type Raio = Float
type Alt = Float
type Raiobase = Float
type Raioalt = Float
type A = Float
type B = Float
type Ex = Float

areaLateral :: Area -> Float
areaLateral (Cilindro raio alt) = 2*pi*raio*alt
areaLateral (Cone raio alt) = pi*raio*(sqrt(raio^2 + alt^2))
areaLateral (TroncoDeCone raiob raioalt alt) = pi*(raiob + raioalt)*(sqrt(alt^2 + (raiob - raioalt)^2))

areaTotal :: Area -> Float
areaTotal (Esfera r) = 4*pi*r^2
areaTotal (Cilindro raio alt) = (2*pi*raio*alt) + (2*pi*raio^2)
areaTotal (Cone raio alt) = pi*raio*(sqrt(raio^2 + alt^2) + raio)
areaTotal (TroncoDeCone raiob raioalt alt) = pi*(raiob + raioalt) + pi*(raiob + raioalt)*(sqrt(alt^2 + (raiob - raioalt)^2))
areaTotal (EsferoideOblato a b ex) = 2 * pi * (a^2) + (b^2 / ((sqrt(a^2 + b^2)/a))) * log((1+((sqrt(a^2 + b^2)/a)))/(1-((sqrt(a^2 + b^2)/a))))
areaTotal (EsferoideProlato a b ex) = 2*pi*(b^2) + 2*pi*((a*b)/(sqrt(a^2 + b^2)/a))*(asin(sqrt(a^2 + b^2)))

vol :: Volume -> Float
vol (Esfera r) = (4/3)*pi*r^2
vol (Cilindro raio alt) =  pi*raio^2*alt
vol (Cone raio alt) =  (1/3)*pi*raio^2*alt
vol (TroncoDeCone raiob raioalt alt) =  (1/3)*pi*alt*(raiob^2 + raioalt^2 + raiob*raioalt)
vol (EsferoideOblato a b ex) =  (4/3)*pi*a^2*b
vol (EsferoideProlato a b ex) =  (4/3)*pi*a*b^2
