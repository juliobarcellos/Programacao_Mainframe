       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGR02T.
       AUTHOR. JULIO CESAR DA SILVA BARCELLOS.
      **************************************
      * MANUTENCAO DO CADASTRO DE FUNCIONARIOS  *
      **************************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                         DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT CADFUN ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS NUMREG
                    FILE STATUS  IS ST-ERRO
                    ALTERNATE RECORD KEY IS NOME  WITH DUPLICATES.
      *
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD CADFUN
            LABEL RECORD IS STANDARD
            VALUE OF FILE-ID IS "CADFUN.DAT".
       01 REGFUN.
                03 NUMREG          PIC 9(06).
                03 NOME            PIC X(30).
                03 DEPARTAMENTO    PIC 9(01).
                03 CARGO           PIC 9(01).
                03 SALARIO         PIC 9(06)V99.
                03 NUMFILHOS       PIC 9(01).
                03 DIAADM          PIC 9(02).
                03 MESADM          PIC 9(02).
                03 ANOADM          PIC 9(04).
                03 DIADEM          PIC 9(02).
                03 MESDEM          PIC 9(02).
                03 ANODEM          PIC 9(04).
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
       01 DEPART     PIC X(22) VALUE SPACES.
       77 CARG       PIC X(20) VALUE SPACES.
       77 IND1       PIC 9(01) VALUE ZEROS.
       77 IND2       PIC 9(02) VALUE ZEROS.
       77 FLAG1      PIC 9(02) VALUE ZEROS.
       77 FLAG2      PIC 9(02) VALUE ZEROS.
       77 ANORESTO   PIC 9(04) VALUE ZEROS.
       77 ANODIV     PIC 9(05) VALUE ZEROS.
       01 TABUNIDADE01.
          03 T01     PIC X(9) VALUE "123456789".
       01 TUN1 REDEFINES TABUNIDADE01.
          03 T1      PIC X(1) OCCURS 9 TIMES.
       01 ALFAUN     PIC X(01) VALUE SPACES.
       01 TABDEP.
          03 DEP1    PIC X(22) VALUE "COMERCIAL".
          03 DEP2    PIC X(22) VALUE "ADMINISTRATIVA".
          03 DEP3    PIC X(22) VALUE "FINANCEIRA".
          03 DEP4    PIC X(22) VALUE "RECURSOS HUMANOS".
          03 DEP5    PIC X(22) VALUE "CONTABILIDADE".
          03 DEP6    PIC X(22) VALUE "TECNOLOGIA INFORMACAO".
          03 DEP7    PIC X(22) VALUE "VENDAS".
          03 DEP8    PIC X(22) VALUE "SERVICOS GERAIS".
          03 DEP9    PIC X(22) VALUE "TRANSPORTES".
       01 TUN1 REDEFINES TABDEP.
           03 TDEP     PIC X(22) OCCURS 09 TIMES.
       01 TABUNIDADE02.
          03 T02     PIC X(10) VALUE "0123456789".
       01 TUN2 REDEFINES TABUNIDADE02.
          03 T2      PIC X(01) OCCURS 10 TIMES.
       01 ALFAUN2    PIC X(02) VALUE SPACES.
       01 TABCAR.
          03 CAR0    PIC X(20) VALUE "DIRETOR".
          03 CAR1    PIC X(20) VALUE "GERENTE".
          03 CAR2    PIC X(20) VALUE "SUPERVISOR".
          03 CAR3    PIC X(20) VALUE "COORDENADOR".
          03 CAR4    PIC X(20) VALUE "ASSISTENTE".
          03 CAR5    PIC X(20) VALUE "AUXILIAR".
          03 CAR6    PIC X(20) VALUE "CONTINUO".
          03 CAR7    PIC X(20) VALUE "VENDEDOR".
          03 CAR8    PIC X(20) VALUE "VIGIA".
          03 CAR9    PIC X(20) VALUE "TELEFONISTA".
       01 TUN02 REDEFINES TABCAR.
           03 TCAR   PIC X(20) OCCURS 10 TIMES.
      *
      *-------------------[ SECAO DE TELA ]------------------------ 
       SCREEN SECTION.
       01  FTELA1.
           05  BLANK SCREEN.
           05  LINE 02  COLUMN 01 
               VALUE  "                            CADASTRO DE".
           05  LINE 02  COLUMN 41 
               VALUE  "FUNCIONARIOS".
           05  LINE 05  COLUMN 01 
               VALUE  "       NUMERO DE REGISTRO:".
           05  LINE 07  COLUMN 01 
               VALUE  "       NOME:".
           05  LINE 09  COLUMN 01 
               VALUE  "       DEPARTAMENTO:".
           05  LINE 11  COLUMN 01 
               VALUE  "       CARGO:".
           05  LINE 13  COLUMN 01 
               VALUE  "       SALARIO BASE:".
           05  LINE 15  COLUMN 01 
               VALUE  "       NUMERO DE FILHOS:".
           05  LINE 17  COLUMN 01 
               VALUE  "       DATA DE ADMISSAO:   /  /".
           05  LINE 18  COLUMN 01 
               VALUE  "       DATA DE DEMISSAO:   /  /".
           05  LINE 21  COLUMN 01 
               VALUE  "       MENSAGEM:".
           05  TNUMREG
               LINE 05  COLUMN 28  PIC 9(06)
               USING  NUMREG.
           05  TNOME
               LINE 07  COLUMN 14  PIC X(30)
               USING  NOME.
           05  TDEPARTAMENTO
               LINE 09  COLUMN 22  PIC 9(01)
               USING  DEPARTAMENTO.
           05  TDEPART
               LINE 09  COLUMN 24  PIC X(22)
               USING  DEPART.
           05  TCARGO
               LINE 11  COLUMN 15  PIC 9(01)
               USING  CARGO.
           05  TCARG
               LINE 11  COLUMN 17  PIC X(20)
               USING  CARG.
           05  TSALARIO
               LINE 13  COLUMN 22  PIC 9(06),99
               USING  SALARIO.
           05  TNUMFILHOS
               LINE 15  COLUMN 26  PIC 9(01)
               USING  NUMFILHOS.
           05  TDIAADM
               LINE 17  COLUMN 26  PIC 9(02)
               USING  DIAADM.
           05  TMESADM
               LINE 17  COLUMN 29  PIC 9(02)
               USING  MESADM.
           05  TANOADM
               LINE 17  COLUMN 32  PIC 9(04)
               USING  ANOADM.
           05  TDIADEM
               LINE 18  COLUMN 26  PIC 9(02)
               USING  DIADEM.
           05  TMESDEM
               LINE 18  COLUMN 29  PIC 9(02)
               USING  MESDEM.
           05  TANODEM
               LINE 18  COLUMN 32  PIC 9(04)
               USING  ANODEM.
           05  TMENS
               LINE 21  COLUMN 18  PIC X(50)
               USING  MENS.
      *
       01  FTELA2.
           05  FOREGROUND-COLOR  1  BACKGROUND-COLOR 0.
           05  LINE 06  COLUMN 41 
               VALUE  "            TABELA DEPARTAMENTOS   ".
           05  LINE 08  COLUMN 41 
               VALUE  "         1- COMERCIAL             ".
           05  LINE 09  COLUMN 41 
               VALUE  "         2- ADMINISTRATIVA        ".
           05  LINE 10  COLUMN 41 
               VALUE  "         3- FINANCEIRA            ".
           05  LINE 11  COLUMN 41 
               VALUE  "         4- RECURSOS HUMANOS      ".
           05  LINE 12  COLUMN 41 
               VALUE  "         5- CONTABILIDADE         ".
           05  LINE 13  COLUMN 41 
               VALUE  "         6- TECNOLOGIA INFORMACAO ".
           05  LINE 14  COLUMN 41 
               VALUE  "         7- VENDAS                ".
           05  LINE 15  COLUMN 41 
               VALUE  "         8- SERVICOS GERAIS       ".
           05  LINE 16  COLUMN 41 
               VALUE  "         9- TRANSPORTES           ".
           05  LINE 18  COLUMN 41 
               VALUE  "                TECLE ESC         ".
           05  LINE 19  COLUMN 41 
               VALUE  "               PARA NAVEGAR       ".
      *
       01  FTELA3.
           05  FOREGROUND-COLOR  1  BACKGROUND-COLOR 0.
           05  LINE 06  COLUMN 41 
               VALUE  "             TABELA CARGOS        ".
           05  LINE 07  COLUMN 41 
               VALUE  "                                  ".
           05  LINE 08  COLUMN 41 
               VALUE  "        0- DIRETOR                ".
           05  LINE 09  COLUMN 41 
               VALUE  "        1- GERENTE                ".
           05  LINE 10  COLUMN 41 
               VALUE  "        2- SUPERVISOR             ".
           05  LINE 11  COLUMN 41 
               VALUE  "        3- COORDENADOR            ".
           05  LINE 12  COLUMN 41 
               VALUE  "        4- ASSISTENTE             ".
           05  LINE 13  COLUMN 41 
               VALUE  "        5- AUXILIAR               ".
           05  LINE 14  COLUMN 41 
               VALUE  "        6- CONTINUO               ".
           05  LINE 15  COLUMN 41 
               VALUE  "        7- VENDEDOR               ".
           05  LINE 16  COLUMN 41 
               VALUE  "        8- VIGIA                  ".
           05  LINE 17  COLUMN 41 
               VALUE  "        9- TELEFONISTA            ".
           05  LINE 18  COLUMN 41 
               VALUE  "               TECLE ESC          ".
           05  LINE 19  COLUMN 41 
               VALUE  "              PARA NAVEGAR        ".
      * 
      *-------------------[ DIVISAO DE PROCEDIMENTOS ]------------------
       PROCEDURE DIVISION.
       INICIO.
      *
       INC-OP0.
           OPEN I-O CADFUN
           IF ST-ERRO NOT = "00"
               IF ST-ERRO = "30"
                      OPEN OUTPUT CADFUN
                      CLOSE CADFUN
                      MOVE "* ARQUIVO CADFUN SENDO CRIADO *" 
                      TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-OP0
                   ELSE
                      MOVE "ERRO NA ABERTURA DO ARQUIVO CADFUN" 
                      TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                ELSE
                    NEXT SENTENCE.
       INC-001.
                MOVE ZEROS  TO NUMREG DEPARTAMENTO CARGO
                               SALARIO NUMFILHOS
                               DIAADM MESADM ANOADM
                               DIADEM MESDEM ANODEM
                MOVE SPACES TO NOME DEPART CARG.
                DISPLAY FTELA1.
       INC-002.
                ACCEPT  TNUMREG
                ACCEPT W-ACT FROM ESCAPE KEY
                 IF W-ACT = 02
                   CLOSE CADFUN
                   GO TO ROT-FIM.
                IF NUMREG  = 0
                   MOVE "*** NÚMERO DE REGISTRO INVALIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-002.
                MOVE 1 TO IND1
                MOVE 1 TO IND2
                MOVE T1(IND1) TO DEPARTAMENTO.
                MOVE TDEP(IND1) TO DEPART.
                MOVE T2(IND2) TO CARGO.
                MOVE TCAR(IND2) TO CARG.
                DISPLAY TDEPARTAMENTO TDEPART TCARGO TCARG.
       LER-CADFUN01.
                MOVE 0 TO W-SEL
                READ CADFUN
                IF ST-ERRO NOT = "23"
                   IF ST-ERRO = "00"
                      MOVE 1 TO W-SEL
                      PERFORM INC-004A THRU INC-004B
                      PERFORM INC-005A THRU INC-005B
                      DISPLAY FTELA1
                      MOVE "*** FUNCIONARIO JA CADASTRADO ***" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      MOVE 1 TO W-SEL
                      GO TO ACE-001
                   ELSE
                      MOVE "ERRO NA LEITURA DO ARQUIVO CADFUN" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                ELSE
                   NEXT SENTENCE.
       INC-003.
                ACCEPT TNOME
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-002.
                IF NOME = SPACES 
                      MOVE "NOME NAO PODE SER BRANCO"   TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-003.
       INC-004.
                DISPLAY FTELA2
                ACCEPT TDEPARTAMENTO
                MOVE TDEP(DEPARTAMENTO) TO DEPART.
                DISPLAY TDEPARTAMENTO TDEPART
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 
                    DISPLAY FTELA1
                    GO TO INC-003.
                IF W-ACT = 01 
                       IF IND1 = 9
                              GO TO INC-100
                       ELSE
                              ADD 1 TO IND1
                              GO TO INC-101.
                IF DEPARTAMENTO = SPACES
                      MOVE "DEPARTAMENTO NAO PODE SER BRANCO"   TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-004.
            GO TO INC-004A.
        INC-100.
            MOVE 1 TO IND1
            MOVE T1(IND1) TO DEPARTAMENTO
            MOVE TDEP(IND1) TO DEPART
            DISPLAY TDEPARTAMENTO TDEPART
            GO TO INC-004.
        INC-101.
            MOVE T1(IND1) TO DEPARTAMENTO
            MOVE TDEP(IND1) TO DEPART
            DISPLAY TDEPARTAMENTO TDEPART
            GO TO INC-004.
       INC-004A.
             MOVE 1 TO IND1.
       INC-004B.
            MOVE T1(IND1) TO ALFAUN
            IF DEPARTAMENTO  NOT = ALFAUN
                    IF IND1 < 9
                             ADD 1 TO IND1
                             GO TO INC-004B
                     ELSE
                        IF W-SEL = 0
                             MOVE "DEPARTAMENTO INVALIDO" TO MENS
                             PERFORM ROT-MENS THRU ROT-MENS-FIM
                             GO TO INC-004
                         ELSE
                             MOVE  ALL "-" TO DEPART
             ELSE
                      MOVE TDEP(IND1) TO DEPART.
              DISPLAY FTELA1.
       INC-005.
                DISPLAY FTELA3
                ACCEPT TCARGO
                MOVE TCAR(IND2) TO CARG.
                DISPLAY TCARGO TCARG
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02
                    DISPLAY FTELA1
                    GO TO INC-004.
                IF W-ACT = 01 
                       IF IND2 = 10
                              MOVE 1 TO IND2
                              MOVE T2(IND2) TO CARGO
                              MOVE TCAR(IND2) TO CARG
                              DISPLAY TCARGO TCARG
                              GO TO INC-005
                       ELSE
                              ADD 1 TO IND2
                              MOVE T2(IND2) TO CARGO
                              MOVE TCAR(IND2) TO CARG
                              DISPLAY TCARGO TCARG
                              GO TO INC-005.
                IF CARGO = SPACES                      
                      MOVE "CARGO NAO PODE SER BRANCO"   TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-005.
       INC-005A.
             MOVE 1 TO IND2.
       INC-005B.
            MOVE T2(IND2) TO ALFAUN2
            IF CARGO  NOT = ALFAUN2
                    IF IND2 < 10
                             ADD 1 TO IND2
                             GO TO INC-005B
                     ELSE
                        IF W-SEL = 0
                             MOVE "CARGO INVALIDO" TO MENS
                             PERFORM ROT-MENS THRU ROT-MENS-FIM
                             GO TO INC-005
                         ELSE
                             MOVE  ALL "-" TO CARG
             ELSE
                      MOVE TCAR(IND2) TO CARG.
              DISPLAY FTELA1.
       INC-006.
                ACCEPT TSALARIO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-005.
                IF SALARIO = ZEROS
                     MOVE "SALARIO NAO PODE SER ZERO"   TO MENS
                     PERFORM ROT-MENS THRU ROT-MENS-FIM
                     GO TO INC-006.
       INC-007.
                ACCEPT TNUMFILHOS
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-006.
       INC-008.
                ACCEPT TDIAADM
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-007.
                MOVE 0 TO FLAG1.
                MOVE SPACES TO MENS.
				DISPLAY TMENS.
       INC-009.
                ACCEPT TMESADM
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-008.
                MOVE 0 TO FLAG1.
       INC-010.
                ACCEPT TANOADM
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-009.
                MOVE 0 TO FLAG1.
                PERFORM VALIDADATA THRU VALIDADATA-FIM
                IF FLAG1 NOT = 0
                    IF FLAG1 = 1
                       GO TO INC-009.
                    IF FLAG1 = 2
                       GO TO INC-008.
                DISPLAY FTELA1.
       INC-011.
                ACCEPT TDIADEM
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-010.
                MOVE 0 TO FLAG2.
       INC-012.
                ACCEPT TMESDEM
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-011.
                MOVE 0 TO FLAG2.
       INC-013.
                ACCEPT TANODEM
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-012.
                MOVE 0 TO FLAG2.
                PERFORM VALIDADATA1 THRU VALIDADATA1-FIM
                IF FLAG2 NOT = 0
                    IF FLAG2 = 1
                       GO TO INC-012.
                    IF FLAG2 = 2
                       GO TO INC-011.
                DISPLAY FTELA1.

      *
                IF W-SEL = 1
                              GO TO ALT-OPC.

       INC-OPC.
                MOVE "S" TO W-OPCAO
                DISPLAY (23, 40) "DADOS OK (S/N) : ".
                ACCEPT (23, 57) W-OPCAO WITH UPDATE
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-013.
                IF W-OPCAO = "N" OR "n"
                   MOVE "* DADOS RECUSADOS PELO OPERADOR *" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM e N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-OPC.
       INC-WR1.
                WRITE REGFUN
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
                DELETE CADFUN RECORD
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
                REWRITE REGFUN
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
       
      *****************************
      *VALIDAÇÃO DE DATA
      *****************************
  
       VALIDADATA.
           IF MESADM > 12
               MOVE "MES INCORRETO, NAO PODE SER MAIOR QUE 12"
               TO MENS
               DISPLAY TMENS
               MOVE "1" TO FLAG1.

           IF  MESADM = 01 OR
               MESADM = 03 OR
               MESADM = 05 OR
               MESADM = 07 OR
               MESADM = 08 OR
               MESADM = 10 OR
               MESADM = 12 AND
               DIAADM > 31
                   MOVE "DIA INCORRETO, NAO PODE SER MAIOR QUE 31"
                   TO MENS
                   DISPLAY TMENS
                   MOVE "2" TO FLAG1.

           IF  MESADM = 04 OR
               MESADM = 06 OR
               MESADM = 09 OR
               MESADM = 11 AND
               DIAADM > 30
                   MOVE "DIA INCORRETO, NAO PODE SER MAIOR QUE 30"
                   TO MENS
                   DISPLAY TMENS
                   MOVE "2" TO FLAG1.
               
           DIVIDE ANOADM BY 4 GIVING ANODIV REMAINDER ANORESTO
           IF MESADM = 02 AND
              ANORESTO = 0 AND
              DIAADM > 29
                  MOVE "DIA INCORRETO, NAO PODE SER MAIOR QUE 29"
                  TO MENS
                  DISPLAY TMENS
                  MOVE "2" TO FLAG1.

           IF MESADM = 02 AND
              ANORESTO NOT = 0 AND
              DIAADM > 28
                  MOVE "DIA INCORRETO, NAO PODE SER MAIOR QUE 28"
                  TO MENS
                  DISPLAY TMENS
                  MOVE "2" TO FLAG1.
       VALIDADATA-FIM.
           EXIT.

       VALIDADATA1.
           IF MESDEM > 12
                  MOVE "MES INCORRETO, NAO PODE SER MAIOR QUE 12"
                  TO MENS
                  DISPLAY TMENS
                  MOVE "1" TO FLAG2.

           IF  MESDEM = 01 OR
               MESDEM = 03 OR
               MESDEM = 05 OR
               MESDEM = 07 OR
               MESDEM = 08 OR
               MESDEM = 10 OR
               MESDEM = 12 AND
               DIADEM > 31
                   MOVE "DIA INCORRETO, NAO PODE SER MAIOR QUE 31"
                   TO MENS
                   DISPLAY TMENS
                   MOVE "2" TO FLAG2.

           IF  MESDEM = 04 OR
               MESDEM = 06 OR
               MESDEM = 09 OR
               MESDEM = 11 AND
               DIADEM > 30
                   MOVE "DIA INCORRETO, NAO PODE SER MAIOR QUE 30"
                   TO MENS
                   DISPLAY TMENS
                   MOVE "2" TO FLAG2.
               
           DIVIDE ANODEM BY 4 GIVING ANODIV REMAINDER ANORESTO
           IF MESDEM = 02 AND
              ANORESTO = 0 AND
              DIADEM > 29
                  MOVE "DIA INCORRETO, NAO PODE SER MAIOR QUE 29"
                  TO MENS
                  DISPLAY TMENS
                  MOVE "2" TO FLAG2.

           IF MESDEM = 02 AND
              ANORESTO NOT = 0 AND
              DIADEM > 28
                  MOVE "DIA INCORRETO, NAO PODE SER MAIOR QUE 28"
                  TO MENS
                  DISPLAY TMENS
                  MOVE "2" TO FLAG2.
       VALIDADATA1-FIM.
           EXIT.