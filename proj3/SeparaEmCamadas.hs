import IO
import Char
import System (getArgs)
import List
import Utils

main = do
	args <- getArgs
	case args of
		[fname] -> do	
			fstr <- readFile fname

			let nome = take (length fname -4) fname
			let nomesBGR = map (nome ++) ["_b.txt", "_g.txt", "_r.txt"]

			--abre os arquivos e armazena os handles em arqsBGR
			arqsBGR <- sequence [openFile f WriteMode | f <- nomesBGR]

			let (cabecalho, imagem) = separaCabecalho fstr
			let (largura, _) = pegaDimensoes cabecalho
			let imagemSemPadding = removePadding imagem largura
			
			let pixels = fazTriplas imagemSemPadding
			let bgr = transpose pixels

			let bgrEmLinhas = map (rearranjaPixelList largura) bgr
			
			--imprime nos arquivos as streams correspondentes
			sequence [escreveEmASCII arq str | (arq, str) <- zip arqsBGR bgrEmLinhas]

			--fecha os arquivos
			sequence [hClose f | f <- arqsBGR]

			putStr "Operacao concluida com sucesso. Os arquivos criados sao: \n "
			putStr $ concat $ map (++ "\n") nomesBGR
	
		_	-> do
				putStr "Execute o programa criado passando como parametro o nome do arquivo a ser separado em camadas\n"
				putStr "Por exemplo: ./programa_compilado vermelho.bmp\n"
