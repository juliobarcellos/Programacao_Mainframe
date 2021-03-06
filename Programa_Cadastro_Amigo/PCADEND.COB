       IDENTIFICATION DIVISION.
       PROGRAM-ID. PCADEND.
       AUTHOR. JULIO CESAR DA SILVA BARCELLOS.
      **************************************
      * MANUTENCAO DO CADASTRO DE ENDEREÇOS  *
      **************************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                         DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT ARQENDER ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS CEP
                    FILE STATUS  IS ST-ERRO
                    ALTERNATE RECORD KEY IS ENDERECO  WITH DUPLICATES.
      *
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD ARQENDER
            LABEL RECORD IS STANDARD
            VALUE OF FILE-ID IS "ARQENDER.DAT".
       01 REGENDER.
                03 CEP             PIC 9(08).
                03 ENDERECO        PIC X(35).
                03 BAIRRO          PIC X(20).
                03 CIDADE          PIC X(35).
                03 UF              PIC X(02).
      *
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
       77 W-SEL      PIC 9(01) VALUE ZEROS.
       77 W-CONT     PIC 9(06) VALUE ZEROS.
       77 W-OPCAO    PIC X(01) VALUE SPACES.
       77 ST-ERRO    PIC X(02) VALUE "00".
       77 W-ACT      PIC 9(02) VALUE ZEROS.
       77 MENS       PIC X(50) VALUE SPACES.
       77 LIMPA      PIC X(50) VALUE SPACES.
       01 ESTADO     PIC X(20) VALUE SPACES.
       77 IND1       PIC 9(01) VALUE ZEROS.
       01 TABUNIDADE01.
          03 T01     PIC X(14) VALUE "SPRJPRRSSCMGMT".
       01 TUN1 REDEFINES TABUNIDADE01.
          03 T1      PIC X(02) OCCURS 07 TIMES.
       01 ALFAUN     PIC X(02) VALUE SPACES.
       01 TABUF.
          03 UF1    PIC X(20) VALUE "SAO PAULO".
          03 UF2    PIC X(20) VALUE "RIO DE JANEIRO".
          03 UF3    PIC X(20) VALUE "PARANA".
          03 UF4    PIC X(20) VALUE "RIO GRANDE DO SUL".
          03 UF5    PIC X(20) VALUE "SANTA CATARINA".
          03 UF6    PIC X(20) VALUE "MINAS GERAIS".
          03 UF7    PIC X(20) VALUE "MATO GROSSO".
       01 TUN1 REDEFINES TABUF.
           03 TUF1     PIC X(20) OCCURS 07 TIMES.
      *
      *-------------------[ SECAO DE TELA ]------------------------ 
       SCREEN SECTION.
       01  FTELA1.
           05  BLANK SCREEN.
           05  LINE 02  COLUMN 01 
               VALUE  "                            CADASTRO DE".
           05  LINE 02  COLUMN 41 
               VALUE  "ENDERECO".
           05  LINE 05  COLUMN 01 
               VALUE  "       CEP:".
           05  LINE 07  COLUMN 01 
               VALUE  "       ENDERECO:".
           05  LINE 09  COLUMN 01 
               VALUE  "       BAIRRO:".
           05  LINE 11  COLUMN 01 
               VALUE  "       CIDADE:".
           05  LINE 13  COLUMN 01 
               VALUE  "       UF:".
           05  LINE 21  COLUMN 01 
               VALUE  "       MENSAGEM:".
           05  TCEP
               LINE 05  COLUMN 13  PIC 9(08)
               USING  CEP.
           05  TENDERECO
               LINE 07  COLUMN 18  PIC X(35)
               USING  ENDERECO.
           05  TBAIRRO
               LINE 09  COLUMN 16  PIC X(20)
               USING  BAIRRO.
           05  TCIDADE
               LINE 11  COLUMN 16  PIC X(35)
               USING  CIDADE.
           05  TUF
               LINE 13  COLUMN 12  PIC X(02)
               USING  UF.
           05  TESTADO
               LINE 13  COLUMN 15  PIC X(20)
               USING  ESTADO.
           05  TMENS
               LINE 21  COLUMN 18  PIC X(50)
               USING  MENS.
      *
       01  FTELA2.
           05  FOREGROUND-COLOR  1  BACKGROUND-COLOR 0.
           05  LINE 06  COLUMN 45 
               VALUE  "            TABELA ESTADOS        ".
           05  LINE 08  COLUMN 45 
               VALUE  "         SP- SAO PAULO            ".
           05  LINE 09  COLUMN 45 
               VALUE  "         RJ- RIO DE JANEIRO       ".
           05  LINE 10  COLUMN 45 
               VALUE  "         PR- PARANA               ".
           05  LINE 11  COLUMN 45 
               VALUE  "         RS- RIO GRANDE DO SUL    ".
           05  LINE 12  COLUMN 45 
               VALUE  "         SC- SANTA CATARINA       ".
           05  LINE 13  COLUMN 45 
               VALUE  "         MG- MINAS GERAIS         ".
           05  LINE 14  COLUMN 45 
               VALUE  "         MT- MATO GROSSO          ".
           05  LINE 15  COLUMN 45 
               VALUE  "                                  ".
           05  LINE 16  COLUMN 45 
               VALUE  "                                  ".
           05  LINE 18  COLUMN 45 
               VALUE  "                TECLE ESC         ".
           05  LINE 19  COLUMN 45 
               VALUE  "               PARA NAVEGAR       ".
      * 
      *-------------------[ DIVISAO DE PROCEDIMENTOS ]------------------
       PROCEDURE DIVISION.
       INICIO.
      *
       INC-OP0.
           OPEN I-O ARQENDER
           IF ST-ERRO NOT = "00"
               IF ST-ERRO = "30"
                      OPEN OUTPUT ARQENDER
                      CLOSE ARQENDER
                      MOVE "* ARQUIVO ARQENDER SENDO CRIADO *" 
                      TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-OP0
                   ELSE
                      MOVE "ERRO NA ABERTURA DO ARQUIVO ARQENDER" 
                      TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                ELSE
                    NEXT SENTENCE.
       INC-001.
                MOVE ZEROS  TO CEP.
                MOVE SPACES TO ENDERECO ESTADO BAIRRO CIDADE
                               UF.
                DISPLAY FTELA1.
       INC-002.
                ACCEPT  TCEP
                ACCEPT W-ACT FROM ESCAPE KEY
                 IF W-ACT = 02
                   CLOSE ARQENDER
                   GO TO ROT-FIM.
                IF CEP  = 0
                   MOVE "*** CEP INVALIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-002.
                MOVE 1 TO IND1.
                MOVE T1(IND1) TO UF.
                MOVE TUF1(IND1) TO ESTADO.
                DISPLAY TUF TESTADO.
       LER-ARQENDER01.
                MOVE 0 TO W-SEL
                READ ARQENDER
                IF ST-ERRO NOT = "23"
                   IF ST-ERRO = "00"
                      MOVE 1 TO W-SEL
                      PERFORM INC-006A THRU INC-006B
                      DISPLAY FTELA1
                      MOVE "*** ENDERECO JA CADASTRADO ***" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      MOVE 1 TO W-SEL
                      GO TO ACE-001
                   ELSE
                      MOVE "ERRO NA LEITURA DO ARQUIVO ARQENDER" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                ELSE
                   NEXT SENTENCE.
       INC-003.
                ACCEPT TENDERECO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-002.
                IF ENDERECO = SPACES 
                      MOVE "ENDERECO NAO PODE SER BRANCO"   TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-003.
       INC-004.
                ACCEPT TBAIRRO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02
                    GO TO INC-003.
                IF BAIRRO = SPACES
                      MOVE "BAIRRO NAO PODE SER BRANCO"   TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-004.
       INC-005.
                ACCEPT TCIDADE
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02
                    GO TO INC-004.
                IF CIDADE = SPACES                      
                      MOVE "CIDADE NAO PODE SER BRANCO"   TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-005.
       INC-006.
                DISPLAY FTELA2
                ACCEPT TUF
                MOVE TUF1(IND1) TO ESTADO.
                DISPLAY TUF TESTADO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 
                    DISPLAY FTELA1
                    GO TO INC-005.
                IF W-ACT = 01 
                       IF IND1 = 7
                              GO TO INC-100
                       ELSE
                              ADD 1 TO IND1
                              GO TO INC-101.
                IF UF = SPACES
                     MOVE "UF NAO PODE SER VAZIO"   TO MENS
                     PERFORM ROT-MENS THRU ROT-MENS-FIM
                     GO TO INC-006.
                GO TO INC-006A.
        INC-100.
            MOVE 1 TO IND1
            MOVE T1(IND1) TO UF
            MOVE TUF1(IND1) TO ESTADO
            DISPLAY TUF TESTADO
            GO TO INC-006.
        INC-101.
            MOVE T1(IND1) TO UF
            MOVE TUF1(IND1) TO ESTADO
            DISPLAY TUF TESTADO
            GO TO INC-006.
       INC-006A.
             MOVE 1 TO IND1.
       INC-006B.
            MOVE T1(IND1) TO ALFAUN
            IF UF  NOT = ALFAUN
                    IF IND1 < 7
                             ADD 1 TO IND1
                             GO TO INC-006B
                     ELSE
                        IF W-SEL = 0
                             MOVE "UF INVALIDA" TO MENS
                             PERFORM ROT-MENS THRU ROT-MENS-FIM
                             GO TO INC-006
                         ELSE
                             MOVE  ALL "-" TO ESTADO
             ELSE
                      MOVE TUF1(IND1) TO ESTADO.
              DISPLAY FTELA1.
       INC-007.
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 
                    DISPLAY FTELA2
                    GO TO INC-006.   

      *
            IF W-SEL = 1
                              GO TO ALT-OPC.

       INC-OPC.
                MOVE "S" TO W-OPCAO
                DISPLAY (23, 40) "DADOS OK (S/N) : ".
                ACCEPT (23, 57) W-OPCAO WITH UPDATE
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-005.
                IF W-OPCAO = "N" OR "n"
                   MOVE "* DADOS RECUSADOS PELO OPERADOR *" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM e N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-OPC.
       INC-WR1.
                WRITE REGENDER
                IF ST-ERRO = "00" OR "02"
                      MOVE "*** DADOS GRAVADOS *** " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-001.
                IF ST-ERRO = "22"
                      MOVE "*** ALUNO JA EXISTE ***       " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-001
                ELSE
                      MOVE "ERRO NA GRAVACAO DO ARQUIVO DE ALUNOS"
                                                       TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM.
      *
      *****************************************
      * ROTINA DE CONSULTA/ALTERACAO/EXCLUSAO *
      *****************************************
      *
       ACE-001.
                DISPLAY (23, 12)
                     "F1=NOVO REGISTRO   F2=ALTERAR   F3=EXCLUIR"
                ACCEPT (23, 55) W-OPCAO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT NOT = 02 AND W-ACT NOT = 03 AND W-ACT NOT = 04
                   GO TO ACE-001.
                MOVE SPACES TO MENS
                DISPLAY (23, 12) MENS
                IF W-ACT = 02
                   MOVE 02 TO W-SEL
                   GO TO INC-001.
                IF W-ACT = 03
                   GO TO INC-003.
      *
       EXC-OPC.
                DISPLAY (23, 40) "EXCLUIR   (S/N) : ".
                ACCEPT (23, 57) W-OPCAO
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** REGISTRO NAO EXCLUIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "* DIGITE APENAS S=SIM  e  N=NAO *" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO EXC-OPC.
       EXC-DL1.
                DELETE ARQENDER RECORD
                IF ST-ERRO = "00"
                   MOVE "*** REGISTRO EXCLUIDO ***        " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO "   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.
      *
       ALT-OPC.
                DISPLAY (23, 40) "ALTERAR  (S/N) : ".
                ACCEPT (23, 57) W-OPCAO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-005.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** INFORMACOES NAO ALTERADAS *** " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "* DIGITE APENAS S=SIM  e  N=NAO *" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ALT-OPC.
       ALT-RW1.
                REWRITE REGENDER
                IF ST-ERRO = "00" OR "02"
                   MOVE "*** REGISTRO ALTERADO ***         " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO PRODUTO"   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.
      *
      **********************
      * ROTINA DE FIM      *
      **********************
      *
       ROT-FIM.
                DISPLAY (01, 01) ERASE
                EXIT PROGRAM.
       ROT-FIMP.
                EXIT PROGRAM.

       ROT-FIMS.
                STOP RUN.
      *
      **********************
      * ROTINA DE MENSAGEM *
      **********************
      *
       ROT-MENS.
                MOVE ZEROS TO W-CONT.
       ROT-MENS1.
               DISPLAY TMENS.
       ROT-MENS2.
                ADD 1 TO W-CONT
                IF W-CONT < 300
                   GO TO ROT-MENS2
                ELSE
                     DISPLAY TMENS.
       ROT-MENS-FIM.
                EXIT.
       FIM-ROT-TEMPO.