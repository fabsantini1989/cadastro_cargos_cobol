       IDENTIFICATION DIVISION.
       PROGRAM-ID. CCG222.
       AUTHOR. FABIANO SANTINI MARQUES.
      ***********************************************
      * MANUTENCAO DO CADASTRO DE FUNCIONARIOS   *
      ***********************************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                     DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT CADCG ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS CODCG
                    FILE STATUS  IS ST-ERRO.
      *
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD CADCG
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADCG.DAT".
       01 REGCG.
                03 CODCG               PIC 9(03).
                03 DENOMI              PIC X(25).
                03 TIPOSA              PIC X(01).
                03 SALARIOB            PIC 9(06)V99.
				03 STATS               PIC X(01).
				03 OBS                 PIC X(40).
      *
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
       77 W-SEL          PIC 9(01) VALUE ZEROS.
       77 W-CONT         PIC 9(06) VALUE ZEROS.
       77 W-OPCAO        PIC X(01) VALUE SPACES.
       77 ST-ERRO        PIC X(02) VALUE "00". 
       77 W-ACT          PIC 9(02) VALUE ZEROS.
       01 TXTIPOSA       PIC X(14) VALUE SPACES.	   
       01 TXSTATS        PIC X(14) VALUE SPACES.
       77 MENS           PIC X(50) VALUE SPACES.
       77 LIMPA          PIC X(50) VALUE SPACES.
      *-----------------------------------------------------------------
       01 TABTIPOSA.
          03 FILLER        PIC X(14) VALUE "HORISTA".
          03 FILLER        PIC X(14) VALUE "DIARISTA".
          03 FILLER        PIC X(14) VALUE "MENSALISTA".
      *		  
       01 TABAUX REDEFINES TABTIPOSA.
           03 TTIPS        PIC X(14) OCCURS 03 TIMES. 		  
      *-----------------------------------------------------------------
       SCREEN SECTION.
       01  TELACARG REQUIRED BACKGROUND-COLOR 7.
	       05 BACKGROUND-COLOR 7.
	       05  BLANK SCREEN.
           05  LINE 24  COLUMN 01 
               VALUE  " _______________________________________"
			   HIGHLIGHT
			   FOREGROUND-COLOR  0.
           05  LINE 24  COLUMN 41 
               VALUE  "________________________________________"
			   HIGHLIGHT
			   FOREGROUND-COLOR  0.
           05  LINE 25  COLUMN 01 
               VALUE  "                                        "
			   BACKGROUND-COLOR 1
			   FOREGROUND-COLOR  6.			   
           05  LINE 25  COLUMN 41 
               VALUE  "             * SANTINI SOLUTIONS (C) *  "
			   BACKGROUND-COLOR 1
			   FOREGROUND-COLOR  15.
           05  LINE 01  COLUMN 01 
               VALUE  "                                        "
			   BACKGROUND-COLOR  1.
           05  LINE 01  COLUMN 41 
               VALUE  "                                        "
			   BACKGROUND-COLOR  1.			   
           05  LINE 02  COLUMN 01 
               VALUE  "                        C A D A S T R O "
			   HIGHLIGHT
			   FOREGROUND-COLOR  15.			   
           05  LINE 02  COLUMN 41 
               VALUE  " D E  C A R G O S                       ".
           05  LINE 03  COLUMN 01 
               VALUE  " _______________________________________"
			   HIGHLIGHT
			   FOREGROUND-COLOR  0.
           05  LINE 03  COLUMN 41 
               VALUE  "________________________________________"
			   HIGHLIGHT
			   FOREGROUND-COLOR  0.
           05  LINE 04  COLUMN 01 
               VALUE  " _______________________________________"
			   HIGHLIGHT
			   FOREGROUND-COLOR  0.
           05  LINE 04  COLUMN 41 
               VALUE  "________________________________________"
			   HIGHLIGHT
			   FOREGROUND-COLOR  0.
           05  LINE 05  COLUMN 01 
               VALUE  "                                        "
			   BACKGROUND-COLOR  7.	
           05  LINE 05  COLUMN 41 
               VALUE  "                                        "
			   BACKGROUND-COLOR  7.				   
           05  LINE 06  COLUMN 30 
               VALUE  "  COD. CARGO :"
			   FOREGROUND-COLOR  6.
           05  LINE 07  COLUMN 01 
               VALUE  " _______________________________________"
			   HIGHLIGHT
			   FOREGROUND-COLOR  0.
           05  LINE 07  COLUMN 41 
               VALUE  "________________________________________"
			   HIGHLIGHT
			   FOREGROUND-COLOR  0.				   
           05  LINE 09  COLUMN 01 
               VALUE  "  DENOMINACAO DO CARGO :"
			   FOREGROUND-COLOR  0.				   
           05  LINE 11  COLUMN 01 
               VALUE  "  TIPO DE SALARIO :"
			   FOREGROUND-COLOR  0.			   
           05  LINE 13  COLUMN 01 
               VALUE  "  SALARIO BASE :"
			   FOREGROUND-COLOR  0.			   
           05  LINE 15  COLUMN 01 
               VALUE  "  STATUS :"
			   FOREGROUND-COLOR  0.			   
           05  LINE 18  COLUMN 01 
               VALUE  "  OBSERVACAO :"
			   FOREGROUND-COLOR  0.			   
           05  LINE  19 COLUMN 01 
               VALUE  " _______________________________________"
			   FOREGROUND-COLOR  0.
           05  LINE 19  COLUMN 41 
               VALUE  "_______________________________________".
           05  TCODCG
               LINE 06  COLUMN 45  PIC 9(03)
               USING  CODCG
			   FOREGROUND-COLOR  6.
           05  TDENOMI
               LINE 09  COLUMN 26  PIC X(25)
               USING  DENOMI
			   FOREGROUND-COLOR  0.	
           05  TTIPOSA
               LINE 11  COLUMN 21  PIC X(01)
               USING  TIPOSA
			   FOREGROUND-COLOR  0.	
           05  TTXTIPOSA
               LINE 11  COLUMN 23  PIC X(14)
               USING  TXTIPOSA
			   FOREGROUND-COLOR  0.				   
           05  TSALARIOB
               LINE 13  COLUMN 18  PIC 9(06)V99
               USING  SALARIOB
			   FOREGROUND-COLOR  0.				   
           05  TSTATS
               LINE 15  COLUMN 12  PIC X(01)
               USING  STATS
			   FOREGROUND-COLOR  0.				   
           05  TTXSTATS
               LINE 15  COLUMN 14  PIC X(14)
               USING  TXSTATS
			   FOREGROUND-COLOR  0.				   
           05  TOBS
               LINE 18  COLUMN 16  PIC X(40)
               USING  OBS
			   FOREGROUND-COLOR  0.				   
      *			   
       01  SCRCRG.	   
	       05  LINE 12  COLUMN 52
               VALUE  "| ******** CARGOS ******* |"
			   BACKGROUND-COLOR 1
			   FOREGROUND-COLOR  15.   
           05  LINE 13  COLUMN 52 
               VALUE  "|     H - HORISTA         |"
			   FOREGROUND-COLOR  0.				   
           05  LINE 14  COLUMN 52 
               VALUE  "|     D - DIARISTA        |"
			   FOREGROUND-COLOR  0.				   
           05  LINE 15  COLUMN 52 
               VALUE  "|     M - MENSALISTA      |"
			   FOREGROUND-COLOR  0.				   
	       05  LINE 16  COLUMN 52
               VALUE  "|_________________________|"
			   FOREGROUND-COLOR  0.		
      *			   
       01  SCRSTATS.	   
	       05  LINE 10  COLUMN 52
               VALUE  "|     *** STATUS ***     |"
			   BACKGROUND-COLOR 1			   
			   FOREGROUND-COLOR  15.
           05  LINE 11  COLUMN 52 
               VALUE  "|     A - ATIVA          |"
			   FOREGROUND-COLOR  0.			   
           05  LINE 12  COLUMN 52 
               VALUE  "|     S - SUSPENSA       |"
			   FOREGROUND-COLOR  0.			   
           05  LINE 13  COLUMN 52 
               VALUE  "|     D - DESATIVADA     |"
			   FOREGROUND-COLOR  0.			   
	       05  LINE 14  COLUMN 52
               VALUE  "|________________________|"
			   FOREGROUND-COLOR  0.			   
      *				   
      *------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO.
     
       R0.
           OPEN I-O CADCG
           IF ST-ERRO NOT = "00"  
              IF ST-ERRO = "30"
                 OPEN OUTPUT CADCG
                 CLOSE CADCG
                 MOVE "*** ARQUIVO CADCG FOI CRIADO **" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R0
              ELSE
                IF ST-ERRO = "95"
                    MOVE "*** ISAM NAO EXCUTADO **" TO MENS
                    PERFORM ROT-MENS THRU ROT-MENS-FIM
                    GO TO ROT-FIM
                ELSE
                    MOVE "ERRO NA ABERTURA DO ARQUIVO CADCG" TO MENS
                    PERFORM ROT-MENS THRU ROT-MENS-FIM
                    GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE.
      * 
      *------------[ INICIALIZACAO DAS VARIAVEIS ]-----------------             
       R1.
           MOVE SPACES TO TXTIPOSA STATS TXSTATS DENOMI
		   MOVE SPACES TO TIPOSA OBS
           MOVE ZEROS TO W-SEL CODCG SALARIOB.
      *-------------[VISUALIZACAO DA TELA]--------------------------------
           DISPLAY TELACARG.
      *-------------[ ENTRADA DA CHAPA ]--------------------------------
       R2.
           DISPLAY (20, 29) "TECLAR ESC PARA SAIR "
           ACCEPT TCODCG
           DISPLAY (20, 29) "                      "
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   CLOSE CADCG
                   GO TO ROT-FIM.
           IF W-ACT = 02
                     MOVE "*** TECLEI O F1 **" TO MENS
                     PERFORM ROT-MENS THRU ROT-MENS-FIM
                     GO TO R2.
           IF W-ACT = 10
                     MOVE "*** TECLEI O F9 **" TO MENS
                     PERFORM ROT-MENS THRU ROT-MENS-FIM
                     GO TO R2.
           IF CODCG = ZEROS
                     MOVE "*** DADO NAO PODE SER ZERO **" TO MENS
                     PERFORM ROT-MENS THRU ROT-MENS-FIM
                     GO TO R2.   
       LER-CADMCG.
           MOVE 0 TO W-SEL
           READ CADCG
           IF ST-ERRO NOT = "23"
              IF ST-ERRO = "00" OR "02"
                DISPLAY TELACARG
                MOVE "*** CARGO JA CADASTRAD0 ***" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ACE-001
             ELSE
                MOVE "ERRO NA LEITURA ARQUIVO CADCG" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM
           ELSE
                MOVE "*** CARGO NAO CADASTRAD0 ***" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM.
       R3.       
           ACCEPT TDENOMI
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R1.
           IF DENOMI = SPACES
                     MOVE "*** DADO CONTEM SO ESPACOS **" TO MENS
                     PERFORM ROT-MENS THRU ROT-MENS-FIM
                     GO TO R3. 				   
       R4.
	       DISPLAY SCRCRG.
           ACCEPT TTIPOSA
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R3.    
         R4A.
           IF TIPOSA = "H"    MOVE "HORISTA" TO TXTIPOSA
           ELSE
		    IF TIPOSA = "D"  MOVE "DIARISTA" TO TXTIPOSA
            ELSE		   
		     IF TIPOSA = "M"  MOVE "MENSALISTA" TO TXTIPOSA
			 ELSE
              GO TO R4.
       R5.
	       DISPLAY TELACARG.
           ACCEPT TSALARIOB.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R4.			   				   		
       R6. 
	       DISPLAY SCRSTATS.
           ACCEPT TSTATS
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R5.
         R6A.
           IF STATS = "A"    MOVE "ATIVA" TO TXSTATS
           ELSE
		    IF STATS = "S"  MOVE "SUSPENSA" TO TXSTATS 
            ELSE		   
		     IF STATS = "D"  MOVE "DESATIVADA" TO TXSTATS
		     ELSE
              GO TO R6.
              DISPLAY TTXSTATS.
       R7.
	       DISPLAY TELACARG.
		   ACCEPT TOBS.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R6.			  
      * ------------- VERICAR SE E ALTERACAO -----------------
           IF W-SEL = 1 
                GO TO ALT-OPC.
       INC-OPC.
                MOVE "S" TO W-OPCAO
                DISPLAY (20, 29) "GRAVAR (S/N) : ".
                ACCEPT (20, 46) W-OPCAO WITH UPDATE
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 01 GO TO R1.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** DADOS RECUSADOS PELO OPERADOR ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM e N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-OPC.
       INC-WR1.
                WRITE REGCG
                IF ST-ERRO = "00"
                      MOVE "*** DADOS GRAVADOS *** " TO MENS
                      PERFORM ROT-MENS THRU
					  ROT-MENS-FIM
                      GO TO R1.
                IF ST-ERRO = "22"
                  MOVE "* CODCG JA EXISTE,DADOS NAO GRAVADOS *"
				  TO MENS
                  PERFORM ROT-MENS
				  THRU ROT-MENS-FIM
                  GO TO ACE-001
                ELSE
                      MOVE "ERRO NA GRAVACAO DO ARQUIVO DE DEPENDENTE" 
					  TO MENS
                      PERFORM ROT-MENS
					  THRU ROT-MENS-FIM
                      GO TO ROT-FIM.

      *
      *****************************************
      * ROTINA DE CONSULTA/ALTERACAO/EXCLUSAO *
      *****************************************
      *
       ACE-001.
	            DISPLAY (20, 02) " N = NOVO REGISTRO".
				DISPLAY (21, 02) " A = ALTERAR REGISTRO".
				DISPLAY (22, 02) " E = EXCLUIR REGISTRO".
                DISPLAY (23, 02) " DIGITE SUA OPCAO: ".
                ACCEPT (23, 21) W-OPCAO
                IF W-OPCAO NOT = "N" AND W-OPCAO NOT = "A" 
                    AND W-OPCAO NOT = "E" GO TO ACE-001.
                MOVE SPACES TO MENS
                DISPLAY (23, 12) MENS
                IF W-OPCAO = "N"
                   GO TO R1  
                ELSE
                   IF W-OPCAO = "A"
                      MOVE 1 TO W-SEL
                      GO TO R3.
      *
       EXC-OPC.
                DISPLAY (22, 35) "      VOCE DESEJA EXCLUIR (S/N)?:".
                ACCEPT (22, 71) W-OPCAO
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** REGISTRO NAO EXCLUIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "* DIGITE APENAS S=SIM  e  N=NAO *" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO EXC-OPC.
       EXC-DL1.
                DELETE CADCG RECORD
                IF ST-ERRO = "00"
                   MOVE "*** REGISTRO CARGO EXCLUIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO "   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.
      *
       ALT-OPC.
                DISPLAY (20, 29) "ALTERAR  (S/N) : ".
                ACCEPT (20, 57) W-OPCAO
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** INFORMACOES NAO ALTERADAS *** " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM  e  N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ALT-OPC.
       ALT-RW1.
                REWRITE REGCG
                IF ST-ERRO = "00" OR "02"
                   MOVE "*** REGISTRO ALTERADO ***         " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                   GO TO ROT-FIM.			
      *-------------------------------------------------------------------------------------------
       ROT-FIM.
           CLOSE CADCG.
           STOP RUN.

      *---------[ ROTINA DE MENSAGEM ]---------------------
       ROT-MENS.
                MOVE ZEROS TO W-CONT.
       ROT-MENS1.
               DISPLAY (20, 26) MENS.
       ROT-MENS2.
                ADD 1 TO W-CONT
                IF W-CONT < 3000
                   GO TO ROT-MENS2
                ELSE
                   MOVE SPACES TO MENS
                   DISPLAY (20, 26) MENS.
       ROT-MENS-FIM.
                EXIT.
       FIM-ROT-TEMPO.