       IDENTIFICATION DIVISION.
       PROGRAM-ID. SMP005.
       AUTHOR. JULIO CESAR DA SILVA BARCELLOS.
      **************************************
      * MANUTENCAO DO CADASTRO DE MEDICOS  *
      **************************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                         DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CADMED ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS CRM
                    FILE STATUS  IS ST-ERRO
                    ALTERNATE RECORD IS NOMEM WITH DUPLICATES.

      *
      *-----------------------------------------------------------------
           SELECT CADCEP ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS CEP
                    FILE STATUS  IS ST-ERRO.
      *
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD CADMED
            LABEL RECORD IS STANDARD
            VALUE OF FILE-ID IS "CADMED.DAT".
       01 REGMED.
                03 CRM             PIC 9(06).
                03 NOMEM            PIC X(30).
                03 LOGRADOURO.
                    05 ACEP        PIC 9(08).
                    05 NUMERO      PIC 9(04).
                    05 COMPLEMENTO PIC X(12).
                03 TELEFONES.
                    05 CELULAR.
                        07 DDDCEL  PIC 9(03).
                        07 NUMCEL  PIC 9(09).
                    05 RESIDENCIAL.
                        07 DDDRES  PIC 9(03).
                        07 NUMRES  PIC 9(08).
                03 EMAIL           PIC X(35).
                03 ESPECIALIDADE   PIC 9(02).
                03 DATANASC.
                    05 DIA         PIC 9(02).
                    05 MES         PIC 9(02).
                    05 ANO         PIC 9(04).
                03 SEXO            PIC X(01).

       FD CADCEP
            LABEL RECORD IS STANDARD
            VALUE OF FILE-ID IS "CADCEP.DAT".
       01 REGCEP.
                03 CEP             PIC 9(08).
                03 ENDERECO        PIC X(30).
                03 BAIRRO          PIC X(20).
                03 CIDADE          PIC X(20).
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
       77 ESPEC      PIC X(20) VALUE SPACES.
       77 SEX        PIC X(09) VALUE SPACES.
       77 IND1       PIC 9(02) VALUE ZEROS.
       77 IND2       PIC 9(02) VALUE ZEROS.
       77 FLAG1      PIC 9(02) VALUE ZEROS.
       77 W-01       PIC 9(03) VALUE ZEROS.
       77 ANORESTO   PIC 9(04) VALUE ZEROS.
       77 ANODIV     PIC 9(05) VALUE ZEROS.

       01 TABUNIDADE01.
          03 T01     PIC X(18) VALUE "010203040506070809".
       01 TUN1 REDEFINES TABUNIDADE01.
          03 T1      PIC X(02) OCCURS 9 TIMES.
       01 ALFAUN     PIC 9(02) VALUE ZEROS.
       01 TABESPEC.
          03 ESP1    PIC X(20) VALUE "CLINICA MEDICA".
          03 ESP2    PIC X(20) VALUE "UROLOGIA".
          03 ESP3    PIC X(20) VALUE "GINECOLOGIA".
          03 ESP4    PIC X(20) VALUE "PEDIATRIA".
          03 ESP5    PIC X(20) VALUE "CARDIOLOGIA".
          03 ESP6    PIC X(20) VALUE "OFTALMOLOGIA".
          03 ESP7    PIC X(20) VALUE "DERMATOLOGIA".
          03 ESP8    PIC X(20) VALUE "NEFROLOGIA".
          03 ESP9    PIC X(20) VALUE "NEUROLOGIA".
       01 TUN1 REDEFINES TABESPEC.
           03 TESP     PIC X(20) OCCURS 09 TIMES.

       01 TABUNIDADE02.
          03 T02     PIC X(02) VALUE "MF".
       01 TUN2 REDEFINES TABUNIDADE02.
          03 T2      PIC X(01) OCCURS 02 TIMES.
       01 ALFAUN2    PIC X(01) VALUE SPACES.
       01 TABSEX.
          03 SEX1    PIC X(09) VALUE "MASCULINO".
          03 SEX2    PIC X(09) VALUE "FEMININO".
       01 TUN02 REDEFINES TABSEX.
           03 TSEX   PIC X(09) OCCURS 02 TIMES.

      *
      *-------------------[ SECAO DE TELA ]------------------------ 
       SCREEN SECTION.
       01  TELAMED.
           05  BLANK SCREEN.
           05  LINE 02  COLUMN 01 
               VALUE  "                            CADASTRO DE".
           05  LINE 02  COLUMN 41 
               VALUE  "MEDICOS".
           05  LINE 04  COLUMN 01 
               VALUE  " CRM:                  NOME:".
           05  LINE 06  COLUMN 01 
               VALUE  " ENDERECO=> CEP:          LOGRADOURO:".
           05  LINE 08  COLUMN 01 
               VALUE  " NUM.:      COMPLEMENTO:              BA".
           05  LINE 08  COLUMN 41 
               VALUE  "IRRO:".
           05  LINE 10  COLUMN 01 
               VALUE  " CIDADE:".
           05  LINE 10  COLUMN 41 
               VALUE  "      UF:".
           05  LINE 12  COLUMN 01 
               VALUE  " TELEFONE=> CELULAR: (   )            RE".
           05  LINE 12  COLUMN 41 
               VALUE  "SIDENCIAL: (   )".
           05  LINE 14  COLUMN 01 
               VALUE  " EMAIL:".
           05  LINE 14  COLUMN 41 
               VALUE  "    DT. NASCIMENTO:   /  /".
           05  LINE 16  COLUMN 01 
               VALUE  " ESPECIALIDADE:                         ".
           05  LINE 16  COLUMN 41 
               VALUE  " SEXO:".
           05  LINE 23  COLUMN 01 
               VALUE  " MENSAGEM".
           05  T-CRM
               LINE 04  COLUMN 11  PIC 9(06)
               USING  CRM.
           05  T-NOMEM
               LINE 04  COLUMN 34  PIC X(30)
               USING  NOMEM.
           05  T-CEP
               LINE 06  COLUMN 18  PIC 9(08)
               USING  ACEP.
           05  T-ENDERECO
               LINE 06  COLUMN 39  PIC X(30)
               USING  ENDERECO.
           05  T-NUM
               LINE 08  COLUMN 08  PIC 9(04)
               USING  NUMERO.
           05  T-COMPL
               LINE 08  COLUMN 26  PIC X(12)
               USING  COMPLEMENTO.
           05  T-BAIRRO
               LINE 08  COLUMN 47  PIC X(20)
               USING  BAIRRO.
           05  T-CID
               LINE 10  COLUMN 10  PIC X(20)
               USING  CIDADE.
           05  T-UF
               LINE 10  COLUMN 51  PIC X(02)
               USING  UF.
           05  T-DDDCEL
               LINE 12  COLUMN 23  PIC 9(03)
               USING  DDDCEL.
           05  T-NUMCEL
               LINE 12  COLUMN 28  PIC 9(09)
               USING  NUMCEL.
           05  T-DDDRES
               LINE 12  COLUMN 53  PIC 9(03)
               USING  DDDRES.
           05  T-NUMRES
               LINE 12  COLUMN 58  PIC 9(08)
               USING  NUMRES.
           05  T-EMAIL
               LINE 14  COLUMN 09  PIC X(35)
               USING  EMAIL.
           05  T-DIA
               LINE 14  COLUMN 61  PIC 9(02)
               USING  DIA.
           05  T-MES
               LINE 14  COLUMN 64  PIC 9(02)
               USING  MES.
           05  T-ANO
               LINE 14  COLUMN 67  PIC 9(04)
               USING  ANO.
           05  T-ESPEC
               LINE 16  COLUMN 17  PIC 9(02)
               USING  ESPECIALIDADE.
           05  T-ESP
               LINE 16  COLUMN 20  PIC X(20)
               USING  ESPEC.
           05  T-SEXO
               LINE 16  COLUMN 48  PIC X(01)
               USING  SEXO.
           05  T-SEX
               LINE 16  COLUMN 50  PIC X(09)
               USING  SEX.
           05  TMENS
               LINE 23  COLUMN 12  PIC X(50)
               USING  MENS.

      *
       01  TELAESP.
           05  FOREGROUND-COLOR  1  BACKGROUND-COLOR 0.
           05  LINE 18  COLUMN 21 
               VALUE  "TABELA ESPECIALIDADE".
           05  LINE 18  COLUMN 41 
               VALUE  " - USE ESC PARA NAVEGAR".
           05  LINE 20  COLUMN 01 
               VALUE  "1- CLINICA MEDICA 2- UROLOGIA 3- GINECOL".
           05  LINE 20  COLUMN 41 
               VALUE  "OGIA 4- PEDIATRIA 5- CARDIOLOGIA       ".
           05  LINE 21  COLUMN 01 
               VALUE  "6- OFTALMOLOGIA 7- DERMATOLOGIA 8- NEFRO".
           05  LINE 21  COLUMN 41 
               VALUE  "LOGIA 9- NEUROLOGIA                   ".

      *
       01 TELASEXO.
           05  FOREGROUND-COLOR  1  BACKGROUND-COLOR 0.
           05  LINE 18  COLUMN 21 
               VALUE  "          TABELA SEX".
           05  LINE 18  COLUMN 41 
               VALUE  "O - USE ESC PARA NAVEGAR".
           05  LINE 20  COLUMN 01 
               VALUE  "                        M- MASCULINO     ".
           05  LINE 20  COLUMN 41 
               VALUE  "   F- FEMININO                           ".
           05  LINE 21  COLUMN 01 
               VALUE  "                                        ".
           05  LINE 21  COLUMN 41 
               VALUE  "                                       ".
      * 
      *-------------------[ DIVISAO DE PROCEDIMENTOS ]------------------
       PROCEDURE DIVISION.
       INICIO.
      *
       INC-OP0.
           OPEN I-O CADMED
           IF ST-ERRO NOT = "00"
               IF ST-ERRO = "30"
                      OPEN OUTPUT CADMED
                      CLOSE CADMED
                      MOVE "* ARQUIVO CADMED SENDO CRIADO *" 
                      TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-OP0
                   ELSE
                      MOVE "ERRO NA ABERTURA DO ARQUIVO CADMED" 
                      TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                ELSE
                    NEXT SENTENCE.

       INC-OP1.
           OPEN INPUT CADCEP
           IF ST-ERRO NOT = "00"
               IF ST-ERRO = "30"
                      MOVE "* ARQUIVO CADCEP NAO EXISTE, FAVOR CRIAR*" 
                      TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                   ELSE
                      MOVE "ERRO NA ABERTURA DO ARQUIVO CADCEP" 
                      TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                ELSE
                    NEXT SENTENCE.

       INC-001.
                MOVE ZEROS  TO CRM ACEP CEP NUMERO
                               TELEFONES ESPECIALIDADE
                               DATANASC IND1 IND2.
                MOVE SPACES TO NOMEM COMPLEMENTO
                               EMAIL SEXO ESPEC SEX
                               ENDERECO BAIRRO CIDADE UF.
                DISPLAY TELAMED.
       INC-002.
                ACCEPT  T-CRM
                ACCEPT W-ACT FROM ESCAPE KEY
                 IF W-ACT = 02
                   CLOSE CADMED
                   GO TO ROT-FIM.
                IF CRM  = ZEROS
                   MOVE "*** CRM NAO PODE SER ZERO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-002.
                MOVE 1 TO IND1
                MOVE 1 TO IND2
                MOVE T1(IND1) TO ESPECIALIDADE.
                MOVE TESP(IND1) TO ESPEC.
                MOVE T2(IND2) TO SEXO.
                MOVE TSEX(IND2) TO SEX.
                DISPLAY T-ESPEC T-ESP T-SEXO T-SEX.
       LER-CADMED01.
                MOVE 0 TO W-SEL
                READ CADMED
                IF ST-ERRO NOT = "23"
                   IF ST-ERRO = "00"
                      MOVE 1 TO W-SEL
                      PERFORM INC-004A THRU INC-004-FIM
                      PERFORM INC-013A THRU INC-013B
                      PERFORM INC-014A THRU INC-014B
                      DISPLAY TELAMED
                      MOVE "*** MEDICO JA CADASTRADO ***" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      MOVE 1 TO W-SEL
                      GO TO ACE-001
                   ELSE
                      MOVE "ERRO NA LEITURA DO ARQUIVO CADMED" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                ELSE
                   NEXT SENTENCE.
       INC-003.
                ACCEPT T-NOMEM
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-002.
                IF NOMEM = SPACES 
                      MOVE "NOME NAO PODE SER BRANCO"   TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-003.
       INC-004.
                ACCEPT T-CEP
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT= 02 GO TO INC-003.
                IF ACEP = ZEROS
                      MOVE "CEP NAO PODE SER ZERO"   TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-004.
       INC-004A.
                MOVE ACEP TO CEP.
                READ CADCEP
                IF ST-ERRO NOT = "00"
                   IF ST-ERRO = "23"
                    IF W-SEL NOT = 1
                     MOVE "CEP NAO CADASTRADO" TO ENDERECO BAIRRO
                     MOVE "CEP NAO CADASTRADO" TO CIDADE
                     MOVE "*** CEP NAO CADASTRADO" TO MENS
                     PERFORM ROT-MENS THRU ROT-MENS-FIM
                     DISPLAY TELAMED
                     GO TO INC-005
                    ELSE
                     MOVE "CEP NAO CADASTRADO" TO ENDERECO BAIRRO
                     MOVE "CEP NAO CADASTRADO" TO CIDADE
                     MOVE "*** CEP NAO CADASTRADO" TO MENS
                     PERFORM ROT-MENS THRU ROT-MENS-FIM
                     DISPLAY TELAMED
                     GO TO INC-004-FIM
                  ELSE
                    MOVE "*** ERRO GRAVE NA LEITURA CADCEP" TO MENS
                    PERFORM ROT-MENS THRU ROT-MENS-FIM
                    GO TO ROT-FIM
                ELSE
                    DISPLAY TELAMED.
       INC-004-FIM.	

       INC-005.
                ACCEPT T-NUM
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT= 02 GO TO INC-004.
                IF NUMERO = ZEROS
                      MOVE "NUMERO NAO PODE SER ZERO"   TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-005.

       INC-006.
                ACCEPT T-COMPL
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT= 02 GO TO INC-005.

       INC-007.
                ACCEPT T-DDDCEL
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT= 02 GO TO INC-006.
                IF DDDCEL = ZEROS
                      MOVE "DDD NAO PODE SER ZERO"   TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-007.

       INC-007A.
                ACCEPT T-NUMCEL
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT= 02 GO TO INC-007.
                IF NUMCEL = ZEROS
                      MOVE "CELULAR NAO PODE SER ZERO"   TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-007A.

       INC-008.
                ACCEPT T-DDDRES
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT= 02 GO TO INC-007A.
                IF DDDRES = ZEROS
                      MOVE "DDD NAO PODE SER ZERO"   TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-008.

       INC-008A.
                ACCEPT T-NUMRES
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT= 02 GO TO INC-008.
                IF NUMRES = ZEROS
                      MOVE "RESIDENCIAL NAO PODE SER ZERO"   TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-008A.

       INC-009.
                ACCEPT T-EMAIL
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT= 02 GO TO INC-008A.
                IF EMAIL = SPACES
                      MOVE "EMAIL NAO PODE SER VAZIO"   TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-009.

       INC-010.
                ACCEPT T-DIA
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-009.
                IF DIA = ZEROS
                      MOVE "DIA NAO PODE SER ZERO"   TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-010.
                MOVE 0 TO FLAG1.
                MOVE SPACES TO MENS.
                DISPLAY TMENS.
       INC-011.
                ACCEPT T-MES
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-010.
                IF MES = ZEROS
                      MOVE "MES NAO PODE SER ZERO"   TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-011.
                MOVE 0 TO FLAG1.
       INC-012.
                ACCEPT T-ANO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-011.
                IF ANO = ZEROS
                      MOVE "ANO NAO PODE SER ZERO"   TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-012.
                MOVE 0 TO FLAG1.
                PERFORM VALIDADATA THRU VALIDADATA-FIM
                IF FLAG1 NOT = 0
                    IF FLAG1 = 1
                       GO TO INC-011.
                    IF FLAG1 = 2
                       GO TO INC-010.
                DISPLAY TELAMED.

       INC-013.
                DISPLAY TELAESP
                ACCEPT T-ESPEC
                MOVE TESP(ESPECIALIDADE) TO ESPEC.
                DISPLAY T-ESPEC T-ESP
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 
                    DISPLAY TELAMED
                    GO TO INC-012.
                IF W-ACT = 01 
                       IF IND1 = 09
                              GO TO INC-100
                       ELSE
                              ADD 01 TO IND1
                              GO TO INC-101.
                IF ESPECIALIDADE = ZEROS
                      MOVE "ESPECIALIDADE NAO PODE SER ZERO"   TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-013.
                GO TO INC-013A.
        INC-100.
            MOVE 01 TO IND1
            MOVE T1(IND1) TO ESPECIALIDADE
            MOVE TESP(IND1) TO ESPEC
            DISPLAY T-ESPEC T-ESP
            GO TO INC-013.
        INC-101.
            MOVE T1(IND1) TO ESPECIALIDADE
            MOVE TESP(IND1) TO ESPEC
            DISPLAY T-ESPEC T-ESP
            GO TO INC-013.
       INC-013A.
             MOVE 1 TO IND1.
       INC-013B.
            MOVE T1(IND1) TO ALFAUN
            IF ESPECIALIDADE  NOT = ALFAUN
                    IF IND1 < 9
                             ADD 1 TO IND1
                             GO TO INC-013B
                     ELSE
                        IF W-SEL = 0
                             MOVE "ESPECIALIDADE INVALIDO" TO MENS
                             PERFORM ROT-MENS THRU ROT-MENS-FIM
                             GO TO INC-013
                         ELSE
                             MOVE  ALL "-" TO ESPEC
             ELSE
                      MOVE TESP(IND1) TO ESPEC.
              DISPLAY TELAESP.

       INC-014.
                DISPLAY TELASEXO
                ACCEPT T-SEXO
                MOVE TSEX(IND2) TO SEX.
                DISPLAY T-SEXO T-SEX
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02
                    DISPLAY TELAESP
                    GO TO INC-013.
                IF W-ACT = 01 
                       IF IND2 = 2
                              MOVE 1 TO IND2
                              MOVE T2(IND2) TO SEXO
                              MOVE TSEX(IND2) TO SEX
                              DISPLAY T-SEXO T-SEX
                              GO TO INC-014
                       ELSE
                              ADD 1 TO IND2
                              MOVE T2(IND2) TO SEXO
                              MOVE TSEX(IND2) TO SEX
                              DISPLAY T-SEXO T-SEX
                              GO TO INC-014.
                IF SEXO = SPACES                      
                      MOVE "SEXO NAO PODE SER BRANCO"   TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-014.
       INC-014A.
             MOVE 1 TO IND2.
       INC-014B.
            MOVE T2(IND2) TO ALFAUN2
            IF SEXO  NOT = ALFAUN2
                    IF IND2 < 2
                             ADD 1 TO IND2
                             GO TO INC-014B
                     ELSE
                        IF W-SEL = 0
                             MOVE "SEXO INVALIDO" TO MENS
                             PERFORM ROT-MENS THRU ROT-MENS-FIM
                             GO TO INC-014
                         ELSE
                             MOVE  ALL "-" TO SEX
             ELSE
                      MOVE TSEX(IND2) TO SEX.
              DISPLAY TELASEXO.

       INC-015.
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 
                    DISPLAY TELASEXO
                    GO TO INC-014.

      *
                IF W-SEL = 1
                              GO TO ALT-OPC.

       INC-OPC.
                MOVE "S" TO W-OPCAO
                DISPLAY (23, 40) "DADOS OK (S/N) : ".
                ACCEPT (23, 57) W-OPCAO WITH UPDATE
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-014.
                IF W-OPCAO = "N" OR "n"
                   MOVE "* DADOS RECUSADOS PELO OPERADOR *" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM e N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-OPC.
       INC-WR1.
                WRITE REGMED
                IF ST-ERRO = "00" OR "02"
                      MOVE "*** DADOS GRAVADOS *** " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-001.
                IF ST-ERRO = "22"
                      MOVE "*** MEDICO JA EXISTE ***       " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-001
                ELSE
                      MOVE "ERRO NA GRAVACAO DO ARQUIVO DE MEDICOS"
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
                DELETE CADMED RECORD
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
                REWRITE REGMED
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
                CLOSE CADMED CADCEP
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
       
      *****************************
      *VALIDAÇÃO DE DATA
      *****************************
  
       VALIDADATA.
           IF MES > 12
               MOVE "MES INCORRETO, NAO PODE SER MAIOR QUE 12"
               TO MENS
               DISPLAY TMENS
               MOVE "1" TO FLAG1.

           IF  MES = 01 OR
               MES = 03 OR
               MES = 05 OR
               MES = 07 OR
               MES = 08 OR
               MES = 10 OR
               MES = 12 AND
               DIA > 31
                   MOVE "DIA INCORRETO, NAO PODE SER MAIOR QUE 31"
                   TO MENS
                   DISPLAY TMENS
                   MOVE "2" TO FLAG1

           ELSE IF  MES = 04 OR
               MES = 06 OR
               MES = 09 OR
               MES = 11 AND
               DIA > 30
                   MOVE "DIA INCORRETO, NAO PODE SER MAIOR QUE 30"
                   TO MENS
                   DISPLAY TMENS
                   MOVE "2" TO FLAG1
           ELSE    
           DIVIDE ANO BY 4 GIVING ANODIV REMAINDER ANORESTO
           IF MES = 02 AND
              ANORESTO = 0 AND
              DIA > 29
                  MOVE "DIA INCORRETO, NAO PODE SER MAIOR QUE 29"
                  TO MENS
                  DISPLAY TMENS
                  MOVE "2" TO FLAG1

           IF MES = 02 AND
              ANORESTO NOT = 0 AND
              DIA > 28
                  MOVE "DIA INCORRETO, NAO PODE SER MAIOR QUE 28"
                  TO MENS
                  DISPLAY TMENS
                  MOVE "2" TO FLAG1.
       VALIDADATA-FIM.
           EXIT.