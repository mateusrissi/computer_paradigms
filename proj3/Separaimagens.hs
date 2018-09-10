module Main where

import IO
import System (getArgs)
import List
import Utils
import Char

main = do
	args <- getArgs
	case args of
		[fname] -> do	
			fstr <- readFile fname

			let nome = take (length fname -4) fname
			let nomesBGR = map (nome ++) ["_b.bmp", "_g.bmp", "_r.bmp"]

			--abre os arquivos e armazena os handles em arqsBGR
			arqsBGR <- sequence [openFile f WriteMode | f <- nomesBGR]

			let (cabecalho, imagem) = separaCabecalho fstr
			let (largura, altura) = pegaDimensoes cabecalho
			
			let imagemSemPadding = removePadding imagem largura
			let pixels = fazTriplas imagemSemPadding
			let bgr = transpose pixels
			
			let zeros = replicate (largura*altura) (chr 0)
			let triplasBlue =  transpose ( [(bgr!!0), zeros   , zeros   ] )
			let triplasGreen = transpose ( [zeros   , (bgr!!1), zeros   ] )
			let triplasRed =   transpose ( [zeros   , zeros   , (bgr!!2)] )
			
			let rawImgStreams = map concat [triplasBlue, triplasGreen, triplasRed]
			let paddedImgStreams = map (adicionaPadding largura) rawImgStreams
			
			let bmpStreams = map (cabecalho ++) paddedImgStreams
			
			--imprime nos arquivos as streams correspondentes
			sequence [hPutStr arq str | (arq, str) <- zip arqsBGR bmpStreams]

			--fecha os arquivos
			sequence [hClose f | f <- arqsBGR]

			putStr "Operacao concluida com sucesso. Os arquivos criados sao: \n "
			putStr $ concat $ map (++ "\n") nomesBGR
	
		_	-> do
				putStr "Execute o programa criado passando como parametro o nome do arquivo a ser separado em camadas\n"
				putStr "Por exemplo: ./programa_compilado vermelho.bmp\n"
