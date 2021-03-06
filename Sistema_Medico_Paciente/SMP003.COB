       IDENTIFICATION DIVISION.
       PROGRAM-ID. SMP003.
       AUTHOR. JULIO CESAR DA SILVA BARCELLOS.
      **************************************
      * MANUTENCAO DO CADASTRO DE CONVENIOS  *
      **************************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                         DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CADCONV ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS CODIGO
                    FILE STATUS  IS ST-ERRO
                    ALTERNATE RECORD IS NOMEC WITH DUPLICATES.

      *
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD CADCONV
            LABEL RECORD IS STANDARD
            VALUE OF FILE-ID IS "CADCONV.DAT".
       01 REGCONV.
                03 CODIGO          PIC 9(04).
                03 NOMEC           PIC X(30).
                03 PLANO           PIC 9(02).

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
       77 PLAN       PIC X(25) VALUE SPACES.
       77 IND1       PIC 9(02) VALUE ZEROS.

       01 TABUNIDADE01.
          03 T01     PIC X(20) VALUE "01020304050607080910".
       01 TUN1 REDEFINES TABUNIDADE01.
          03 T1      PIC X(02) OCCURS 10 TIMES.
       01 ALFAUN     PIC 9(02) VALUE ZEROS.
       01 TABPLANO.
          03 P1      PIC X(25) VALUE "ENFERMARIA REGIONAL".
          03 P2      PIC X(25) VALUE "ENFERMARIA NACIONAL".
          03 P3      PIC X(25) VALUE "ENFERMARIA INTERNACIONAL".
          03 P4      PIC X(25) VALUE "APTO PADRAO REGIONAL".
          03 P5      PIC X(25) VALUE "APTO PADRAO NACIONAL".
          03 P6      PIC X(25) VALUE "APTO PADRAO INTERNACIONAL".
          03 P7      PIC X(25) VALUE "EMERGENCIA REGIONAL".
          03 P8      PIC X(25) VALUE "EMERGENCIA NACIONAL".
          03 P9      PIC X(25) VALUE "EMERGENCIA INTERNACIONAL".
          03 P10     PIC X(25) VALUE "PLANO GLOBAL".
       01 TUN1 REDEFINES TABPLANO.
           03 TPLANO     PIC X(25) OCCURS 10 TIMES.

      *
      *-------------------[ SECAO DE TELA ]------------------------ 
       SCREEN SECTION.
       01  TELACONV.
           05  BLANK SCREEN.
           05  LINE 02  COLUMN 01 
               VALUE  "                            CADASTRO DE".
           05  LINE 02  COLUMN 41 
               VALUE  "CONVENIO".
           05  LINE 04  COLUMN 01 
               VALUE  " CODIGO:".
           05  LINE 06  COLUMN 01 
               VALUE  " NOME:".
           05  LINE 08  COLUMN 01 
               VALUE  " PLANO:".
           05  LINE 23  COLUMN 01 
               VALUE  " MENSAGEM".
           05  T-CODIGO
               LINE 04  COLUMN 10  PIC 9(04)
               USING  CODIGO.
           05  T-NOMEC
               LINE 06  COLUMN 08  PIC X(30)
               USING  NOMEC.
           05  T-PLANO
               LINE 08  COLUMN 09  PIC 9(02)
               USING  PLANO.
           05  T-PLAN
               LINE 08  COLUMN 12  PIC X(25)
               USING PLAN.
           05  TMENS
               LINE 23  COLUMN 12  PIC X(50)
               USING  MENS.

      *
       01  TELAPLAN.
          05  LINE 10  COLUMN 01 
               VALUE  "                               TABELA DE".
           05  LINE 10  COLUMN 41 
               VALUE  " PLANOS".
           05  LINE 12  COLUMN 01 
               VALUE  "                            TECLE ESC PA".
           05  LINE 12  COLUMN 41 
               VALUE  "RA NAVEGAR".
           05  LINE 14  COLUMN 01 
               VALUE  "  1- ENFERMARIA REGIONAL  2- ENFERMARIA".
           05  LINE 14  COLUMN 41 
               VALUE  "NACIONAL  3- ENFERMARIA INTERNACIONAL".
           05  LINE 16  COLUMN 01 
               VALUE  "           4- APTO PADRAO REGIONAL  5- A".
           05  LINE 16  COLUMN 41 
               VALUE  "PTO PADRAO NACIONAL".
           05  LINE 18  COLUMN 01 
               VALUE  "  6- APTO PADRAO INTERNACIONAL  7- EMERG".
           05  LINE 18  COLUMN 41 
               VALUE  "ENCIA REGIONAL  8- EMERGENCIA NACIONAL".
           05  LINE 20  COLUMN 01 
               VALUE  "           9- EMERGENCIA INTERNACIONAL".
           05  LINE 20  COLUMN 41 
               VALUE  "10- PLANO GLOBAL".

      * 
      *-------------------[ DIVISAO DE PROCEDIMENTOS ]------------------
       PROCEDURE DIVISION.
       INICIO.
      *
       INC-OP0.
           OPEN I-O CADCONV
           IF ST-ERRO NOT = "00"
               IF ST-ERRO = "30"
                      OPEN OUTPUT CADCONV
                      CLOSE CADCONV
                      MOVE "* ARQUIVO CADCONV SENDO CRIADO *" 
                      TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-OP0
                   ELSE
                      MOVE "ERRO NA ABERTURA DO ARQUIVO CADCONV" 
                      TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                ELSE
                    NEXT SENTENCE.

       INC-001.
                MOVE ZEROS  TO CODIGO PLANO.
                MOVE SPACES TO NOMEC PLAN.
                DISPLAY TELACONV.
       INC-002.
                ACCEPT  T-CODIGO
                ACCEPT W-ACT FROM ESCAPE KEY
                 IF W-ACT = 02
                   CLOSE CADCONV
                   GO TO ROT-FIM.
                IF CODIGO  = ZEROS
                   MOVE "*** CODIGO NAO PODE SER ZERO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-002.
                MOVE 1 TO IND1
                MOVE T1(IND1) TO PLANO.
                MOVE TPLANO(IND1) TO PLAN.
                DISPLAY T-PLANO T-PLAN.
       LER-CADMED01.
                MOVE 0 TO W-SEL
                READ CADCONV
                IF ST-ERRO NOT = "23"
                   IF ST-ERRO = "00"
                      MOVE 1 TO W-SEL
                      PERFORM INC-004A THRU INC-004B
                      DISPLAY TELACONV
                      MOVE "*** CONVENIO JA CADASTRADO ***" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      MOVE 1 TO W-SEL
                      GO TO ACE-001
                   ELSE
                      MOVE "ERRO NA LEITURA DO ARQUIVO CADCONV" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                ELSE
                   NEXT SENTENCE.
       INC-003.
                ACCEPT T-NOMEC
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-002.
                IF NOMEC = SPACES 
                      MOVE "NOME NAO PODE SER BRANCO"   TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-003.
       INC-004.
                DISPLAY TELAPLAN
                ACCEPT T-PLANO
                MOVE TPLANO(PLANO) TO PLAN.
                DISPLAY T-PLANO T-PLAN
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 
                    DISPLAY TELACONV
                    GO TO INC-003.
                IF W-ACT = 01 
                       IF IND1 = 10
                              GO TO INC-100
                       ELSE
                              ADD 01 TO IND1
                              GO TO INC-101.
                IF PLANO = ZEROS
                      MOVE "PLANO NAO PODE SER ZERO"   TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-004.
                GO TO INC-004A.
        INC-100.
            MOVE 01 TO IND1
            MOVE T1(IND1) TO PLANO
            MOVE TPLANO(IND1) TO PLAN
            DISPLAY T-PLANO T-PLAN
            GO TO INC-004.
        INC-101.
            MOVE T1(IND1) TO PLANO
            MOVE TPLANO(IND1) TO PLAN
            DISPLAY T-PLANO T-PLAN
            GO TO INC-004.
       INC-004A.
             MOVE 1 TO IND1.
       INC-004B.
            MOVE T1(IND1) TO ALFAUN
            IF PLANO  NOT = ALFAUN
                    IF IND1 < 10
                             ADD 1 TO IND1
                             GO TO INC-004B
                     ELSE
                        IF W-SEL = 0
                             MOVE "PLANO INVALIDO" TO MENS
                             PERFORM ROT-MENS THRU ROT-MENS-FIM
                             GO TO INC-004
                         ELSE
                             MOVE  ALL "-" TO PLAN
             ELSE
                      MOVE TPLANO(IND1) TO PLAN.
              DISPLAY TELAPLAN.
       INC-015.
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 
                    GO TO INC-004.

      *
                IF W-SEL = 1
                              GO TO ALT-OPC.

       INC-OPC.
                MOVE "S" TO W-OPCAO
                DISPLAY (23, 40) "DADOS OK (S/N) : ".
                ACCEPT (23, 57) W-OPCAO WITH UPDATE
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-004.
                IF W-OPCAO = "N" OR "n"
                   MOVE "* DADOS RECUSADOS PELO OPERADOR *" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM e N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-OPC.
       INC-WR1.
                WRITE REGCONV
                IF ST-ERRO = "00" OR "02"
                      MOVE "*** DADOS GRAVADOS *** " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-001.
                IF ST-ERRO = "22"
                      MOVE "*** CONVENIO JA EXISTE ***       " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-001
                ELSE
                      MOVE "ERRO NA GRAVACAO DO ARQUIVO DE CONVENIOS"
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
                DELETE CADCONV RECORD
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
                IF W-ACT = 02 GO TO INC-004.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** INFORMACOES NAO ALTERADAS *** " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "* DIGITE APENAS S=SIM  e  N=NAO *" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ALT-OPC.
       ALT-RW1.
                REWRITE REGCONV
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
                CLOSE CADCONV
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
                IF W-CONT < 3000
                   GO TO ROT-MENS2
                ELSE
                     MOVE LIMPA TO MENS
                     DISPLAY TMENS.
       ROT-MENS-FIM.
                EXIT.
       FIM-ROT-TEMPO.