       IDENTIFICATION DIVISION.
       PROGRAM-ID. SMP006.
       AUTHOR. JULIO CESAR DA SILVA BARCELLOS.
      **************************************
      * MANUTENCAO DO CADASTRO DE CONSULTAS  *
      **************************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                         DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CADCONSU ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS CPFDATA
                    FILE STATUS  IS ST-ERRO.
      *
      *-----------------------------------------------------------------
           SELECT CADMED ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS CRM
                    FILE STATUS  IS ST-ERRO-MED.
      *
      *-----------------------------------------------------------------
           SELECT CADCID ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS CODCID
                    FILE STATUS  IS ST-ERRO-CID.
      *
      *-----------------------------------------------------------------
           SELECT CADCONV ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS CODIGO
                    FILE STATUS  IS ST-ERRO-CONV.
      *
      *-----------------------------------------------------------------
           SELECT CADPACI ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS CPF
                    FILE STATUS  IS ST-ERRO-PACI.
      *
      *-----------------------------------------------------------------

       DATA DIVISION.
       FILE SECTION.
       FD CADCONSU
            LABEL RECORD IS STANDARD
            VALUE OF FILE-ID IS "CADCONSU.DAT".
       01 REGCONSU.
                03 CPFDATA.
                    05 CPF-P       PIC 9(11).
                    05 DATACONSULTA.
                        07 DIA-C   PIC 9(02).
                        07 MES-C   PIC 9(02).
                        07 ANO-C   PIC 9(04).
                03 CRM-MED         PIC 9(06).
                03 CID             PIC 9(04).
                03 DESCRCON1       PIC X(60).
                03 DESCRCON2       PIC X(60).

       FD CADMED
            LABEL RECORD IS STANDARD
            VALUE OF FILE-ID IS "CADMED.DAT".
       01 REGMED.
                03 CRM             PIC 9(06).
                03 NOME-M          PIC X(30).
                03 LOGRADOURO.
                    05 ACEP        PIC 9(08).
                    05 NUMERO      PIC 9(04).
                    05 COMPLEMENTO PIC X(12).
                03 TELEFONES.
                    05 CELULAR.
                        07 DDDCEL  PIC 9(02).
                        07 NUMCEL  PIC 9(09).
                    05 RESIDENCIAL.
                        07 DDDRES  PIC 9(03).
                        07 NUMRES  PIC 9(08).
                03 EMAIL-M         PIC X(35).
                03 ESPECIALIDADE   PIC 9(02).
                03 DATANASC-M.
                    05 DIA-M       PIC 9(02).
                    05 MES-M       PIC 9(02).
                    05 ANO-M       PIC 9(04).
                03 SEXO-M          PIC X(01).

       FD CADCID
            LABEL RECORD IS STANDARD
            VALUE OF FILE-ID IS "CADCID.DAT".
       01 REGCID.
                03 CODCID          PIC 9(04).
                03 DENOMINACAO     PIC X(30).

       FD CADCONV
            LABEL RECORD IS STANDARD
            VALUE OF FILE-ID IS "CADCONV.DAT".
       01 REGCONV.
                03 CODIGO          PIC 9(04).
                03 NOMEC           PIC X(30).
                03 PLANO           PIC 9(02).

       FD CADPACI
            LABEL RECORD IS STANDARD
            VALUE OF FILE-ID IS "CADPACI.DAT".
       01 REGPACI.
                03 CPF             PIC 9(11).
                03 NOME-P          PIC X(30).
                03 DATANASC-P.
                    05 DIA-P       PIC 9(02).
                    05 MES-P       PIC 9(02).
                    05 ANO-P       PIC 9(04).
                03 SEXO-P          PIC X(01).
                03 GENERO-P        PIC X(01).
                03 CONVENIO        PIC 9(04).
                03 LOGRADOURO.
                    05 ACEP        PIC 9(08).
                    05 NUMCASA     PIC 9(04).
                    05 COMPLEMENTO PIC X(10).
                03 TELEFONE.
                        07 DDD     PIC 9(02).
                        07 NUMTEL  PIC 9(09).
                03 EMAIL-P         PIC X(30).
      *
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
       77 W-SEL        PIC 9(01) VALUE ZEROS.
       77 W-CONT       PIC 9(06) VALUE ZEROS.
       77 W-OPCAO      PIC X(01) VALUE SPACES.
       77 ST-ERRO      PIC X(02) VALUE "00".
       77 ST-ERRO-MED  PIC X(02) VALUE "00".
       77 ST-ERRO-CID  PIC X(02) VALUE "00".
       77 ST-ERRO-CONV PIC X(02) VALUE "00".
       77 ST-ERRO-PACI PIC X(02) VALUE "00".
       77 W-ACT        PIC 9(02) VALUE ZEROS.
       77 MENS         PIC X(50) VALUE SPACES.
       77 LIMPA        PIC X(50) VALUE SPACES.
       77 ESPEC        PIC X(20) VALUE SPACES.
       77 GEN-P        PIC X(20) VALUE SPACES.
       77 SEX-P        PIC X(09) VALUE SPACES.
       77 SEX-M        PIC X(09) VALUE SPACES.
       77 PLAN         PIC X(25) VALUE SPACES.
       77 IND1         PIC 9(01) VALUE ZEROS.
       77 IND2         PIC 9(01) VALUE ZEROS.
       77 IND3         PIC 9(01) VALUE ZEROS.
       77 IND4         PIC 9(02) VALUE ZEROS.
       77 IND5         PIC 9(02) VALUE ZEROS.
       77 FLAG1        PIC 9(02) VALUE ZEROS.
       77 W-01         PIC 9(03) VALUE ZEROS.
       77 ANORESTO     PIC 9(04) VALUE ZEROS.
       77 ANODIV       PIC 9(05) VALUE ZEROS.

       01 TABUNIDADE01.
          03 T01     PIC X(06) VALUE "NHBTPO".
       01 TUN1 REDEFINES TABUNIDADE01.
          03 T1      PIC X(01) OCCURS 6 TIMES.
       01 ALFAUN     PIC 9(01) VALUE ZEROS.
       01 TABGEN.
          03 GEN1    PIC X(20) VALUE "NAO DECLARADO".
          03 GEN2    PIC X(20) VALUE "HETEROSEXUAL".
          03 GEN3    PIC X(20) VALUE "BISEXUAL".
          03 GEN4    PIC X(20) VALUE "TRANSEXUAL".
          03 GEN5    PIC X(20) VALUE "PANSEXUAL".
          03 GEN6    PIC X(20) VALUE "OUTROS".
       01 TUN01 REDEFINES TABGEN.
           03 TGEN     PIC X(20) OCCURS 06 TIMES.

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

       01 TABUNIDADE03.
          03 T03     PIC X(20) VALUE "01020304050607080910".
       01 TUN3 REDEFINES TABUNIDADE03.
          03 T3      PIC X(02) OCCURS 10 TIMES.
       01 ALFAUN3     PIC 9(02) VALUE ZEROS.
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
       01 TUN03 REDEFINES TABPLANO.
           03 TPLANO     PIC X(25) OCCURS 10 TIMES.

       01 TABUNIDADE04.
          03 T04     PIC X(18) VALUE "010203040506070809".
       01 TUN4 REDEFINES TABUNIDADE04.
          03 T4      PIC X(02) OCCURS 9 TIMES.
       01 ALFAUN4     PIC 9(02) VALUE ZEROS.
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
       01 TUN04 REDEFINES TABESPEC.
           03 TESP     PIC X(20) OCCURS 09 TIMES.
      *
      *-------------------[ SECAO DE TELA ]------------------------ 
       SCREEN SECTION.
       01  TELACON.
           05  BLANK SCREEN.
           05  LINE 02  COLUMN 01 
               VALUE  "                            CADASTRO DE".
           05  LINE 02  COLUMN 41 
               VALUE  "CONSULTAS".
           05  LINE 04  COLUMN 01 
               VALUE  "PACIENTE:                            DAT".
           05  LINE 04  COLUMN 41 
               VALUE  "A CONSULTA:   /  /".
           05  LINE 06  COLUMN 01 
               VALUE  "NOME PACIENTE:".
           05  LINE 07  COLUMN 01 
               VALUE  "DATA NASCIMENTO:   /  /       SEXO:".
           05  LINE 07  COLUMN 41 
               VALUE  "        GENERO:".
           05  LINE 09  COLUMN 01 
               VALUE  "CONVENIO:      -".
           05  LINE 10  COLUMN 01 
               VALUE  "PLANO:         -".
           05  LINE 12  COLUMN 01 
               VALUE  "MEDICO =>".
           05  LINE 13  COLUMN 01 
               VALUE  "CRM:         NOME:".
           05  LINE 13  COLUMN 41 
               VALUE  "           SEXO:".
           05  LINE 14  COLUMN 01 
               VALUE  "ESPECIALIDADE:    -".
           05  LINE 16  COLUMN 01 
               VALUE  "CONSULTA=>".
           05  LINE 17  COLUMN 01 
               VALUE  "CID:         DENOMINACAO:".
           05  LINE 18  COLUMN 01 
               VALUE  "DESCRICAO 1:".
           05  LINE 19  COLUMN 01 
               VALUE  "DESCRICAO 2:".
           05  LINE 23  COLUMN 01 
               VALUE  "MENSAGEM:".
           05  T-CPFP
               LINE 04  COLUMN 11  PIC 9(11)
               USING  CPF-P.
           05  T-DIAC
               LINE 04  COLUMN 53  PIC 9(02)
               USING  DIA-C.
           05  T-MESC
               LINE 04  COLUMN 56  PIC 9(02)
               USING  MES-C.
           05  T-ANOC
               LINE 04  COLUMN 59  PIC 9(04)
               USING  ANO-C.
           05  T-NOMEP
               LINE 06  COLUMN 16  PIC X(30)
               USING  NOME-P.
           05  T-DIAP
               LINE 07  COLUMN 18  PIC 9(02)
               USING  DIA-P.
           05  T-MESP
               LINE 07  COLUMN 21  PIC 9(02)
               USING  MES-P.
           05  T-ANOP
               LINE 07  COLUMN 24  PIC 9(04)
               USING  ANO-P.
           05  T-SEXO-P
               LINE 07  COLUMN 37  PIC X(01)
               USING  SEXO-P.
           05  T-SEX-P
               LINE 07  COLUMN 39  PIC X(09)
               USING  SEX-P.
           05  T-GENERO-P
               LINE 07  COLUMN 57  PIC X(01)
               USING  GENERO-P.
           05  T-GEN-P
               LINE 07  COLUMN 59  PIC X(20)
               USING  GEN-P.
           05  T-CONVENIO
               LINE 09  COLUMN 11  PIC 9(04)
               USING  CONVENIO.
           05  T-NOMEC
               LINE 09  COLUMN 18  PIC X(30)
               USING  NOMEC.
           05  T-PLANO
               LINE 10  COLUMN 13  PIC 9(02)
               USING  PLANO.
           05  T-PLAN
               LINE 10  COLUMN 18  PIC X(25)
               USING  PLAN.
           05  T-CRM
               LINE 13  COLUMN 06  PIC 9(06)
               USING  CRM-MED.
           05  T-NOME-M
               LINE 13  COLUMN 20  PIC X(30)
               USING  NOME-M.
           05  T-SEXO-M
               LINE 13  COLUMN 58  PIC X(01)
               USING  SEXO-M.
           05  T-SEXM
               LINE 13  COLUMN 60  PIC X(09)
               USING  SEX-M.
           05  T-ESPECIALIDADE
               LINE 14  COLUMN 16  PIC 9(02)
               USING  ESPECIALIDADE.
           05  T-ESPEC
               LINE 14  COLUMN 21  PIC X(20)
               USING  ESPEC.
           05  T-CID
               LINE 17  COLUMN 06  PIC 9(04)
               USING  CID.
           05  T-DENOMINACAO
               LINE 17  COLUMN 27  PIC X(30)
               USING  DENOMINACAO.
           05  T-DESCR1
               LINE 18  COLUMN 14  PIC X(60)
               USING  DESCRCON1.
           05  T-DESCR2
               LINE 19  COLUMN 14  PIC X(60)
               USING  DESCRCON2.
           05  TMENS
               LINE 23  COLUMN 11  PIC X(50)
               USING  MENS.

      * 
      *-------------------[ DIVISAO DE PROCEDIMENTOS ]------------------
       PROCEDURE DIVISION.
       INICIO.
      *
       INC-OP0.
           OPEN I-O CADCONSU
           IF ST-ERRO NOT = "00"
               IF ST-ERRO = "30"
                      OPEN OUTPUT CADCONSU
                      CLOSE CADCONSU
                      MOVE "* ARQUIVO CADCONSU SENDO CRIADO *" 
                      TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-OP0
                   ELSE
                      MOVE "ERRO NA ABERTURA DO ARQUIVO CADCONSU" 
                      TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                ELSE
                    NEXT SENTENCE.

       INC-OP1.
           OPEN INPUT CADMED
           IF ST-ERRO NOT = "00"
               IF ST-ERRO = "30"
                      MOVE "* ARQUIVO CADMED NAO EXISTE, FAVOR CRIAR*" 
                      TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                   ELSE
                      MOVE "ERRO NA ABERTURA DO ARQUIVO CADMED" 
                      TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                ELSE
                    NEXT SENTENCE.

       INC-OP2.
           OPEN INPUT CADCID
           IF ST-ERRO NOT = "00"
               IF ST-ERRO = "30"
                      MOVE "* ARQUIVO CADCID NAO EXISTE, FAVOR CRIAR*" 
                      TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                   ELSE
                      MOVE "ERRO NA ABERTURA DO ARQUIVO CADCID" 
                      TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                ELSE
                    NEXT SENTENCE.

       INC-OP3.
           OPEN INPUT CADCONV
           IF ST-ERRO NOT = "00"
               IF ST-ERRO = "30"
                      MOVE "* ARQUIVO CADCONV NAO EXISTE, FAVOR CRIAR*" 
                      TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                   ELSE
                      MOVE "ERRO NA ABERTURA DO ARQUIVO CADCONV" 
                      TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                ELSE
                    NEXT SENTENCE.

       INC-OP4.
           OPEN INPUT CADPACI
           IF ST-ERRO NOT = "00"
               IF ST-ERRO = "30"
                      MOVE "* ARQUIVO CADPACI NAO EXISTE, FAVOR CRIAR*" 
                      TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                   ELSE
                      MOVE "ERRO NA ABERTURA DO ARQUIVO CADPACI" 
                      TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                ELSE
                    NEXT SENTENCE.

       INC-001.
                MOVE ZEROS  TO CPFDATA CRM-MED
                               CID IND1 IND2 IND3 IND4.
                MOVE SPACES TO DESCRCON1 DESCRCON2 SEX-M SEX-P
                               GEN-P PLAN ESPEC.
                DISPLAY TELACON.
       INC-002.
                ACCEPT  T-CPFP
                ACCEPT W-ACT FROM ESCAPE KEY
                 IF W-ACT = 02
                   CLOSE CADCONSU
                   GO TO ROT-FIM.
                IF CPF-P  = ZEROS
                   MOVE "*** CPF NAO PODE SER ZERO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-002.
       INC-002A.
                ACCEPT  T-DIAC
                ACCEPT W-ACT FROM ESCAPE KEY
                 IF W-ACT = 02
                   GO TO INC-002.
                IF DIA-C  = ZEROS
                   MOVE "*** DIA NAO PODE SER ZERO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-002A.
       INC-002B.
                ACCEPT  T-MESC
                ACCEPT W-ACT FROM ESCAPE KEY
                 IF W-ACT = 02
                   GO TO INC-002A.
                IF MES-C  = ZEROS
                   MOVE "*** MES NAO PODE SER ZERO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-002B.
       INC-002C.
                ACCEPT  T-ANOC
                ACCEPT W-ACT FROM ESCAPE KEY
                 IF W-ACT = 02
                   GO TO INC-002B.
                IF ANO-C  = ZEROS
                   MOVE "*** ANO NAO PODE SER ZERO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-002C.
                MOVE 1 TO IND1
                MOVE 1 TO IND2
                MOVE 1 TO IND3
                MOVE 1 TO IND4
                MOVE 1 TO IND5
                MOVE T1(IND1) TO GENERO-P.
                MOVE TGEN(IND1) TO GEN-P.
                MOVE T2(IND2) TO SEXO-P.
                MOVE TSEX(IND2) TO SEX-P.
                MOVE T3(IND3) TO PLANO.
                MOVE TPLANO(IND3) TO PLAN.
                MOVE T2(IND4) TO SEXO-M.
                MOVE TSEX(IND4) TO SEX-M.
                MOVE T4(IND5) TO ESPECIALIDADE.
                MOVE TESP(IND5) TO ESPEC.
                DISPLAY T-GENERO-P T-GEN-P T-SEXO-P T-SEX-P
                        T-PLANO T-PLAN T-ESPECIALIDADE T-ESPEC.
       LER-CADCONSU01.
                MOVE 0 TO W-SEL
                READ CADCONSU
                IF ST-ERRO NOT = "23"
                   IF ST-ERRO = "00"
                      MOVE 1 TO W-SEL
                      DISPLAY TELACON
                      MOVE "*** CONSULTA JA CADASTRADA ***" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      MOVE 1 TO W-SEL
                      GO TO ACE-001
                   ELSE
                      MOVE "ERRO NA LEITURA DO ARQUIVO CADCONSU" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                ELSE
                   NEXT SENTENCE.
       LER-CADPACI.
                MOVE CPF-P TO CPF.
                READ CADPACI
                IF SEXO-P = "M"
                   MOVE TSEX(1) TO SEX-P
                ELSE
                   MOVE TSEX(2) TO SEX-P
                IF GENERO-P = "N"
                   MOVE TGEN(1) TO GEN-P
                ELSE IF GENERO-P = "H"
                   MOVE TGEN(2) TO GEN-P
                ELSE IF GENERO-P = "B"
                   MOVE TGEN(3) TO GEN-P
                ELSE IF GENERO-P = "T"
                   MOVE TGEN(4) TO GEN-P
                ELSE IF GENERO-P = "P"
                   MOVE TGEN(5) TO GEN-P
                ELSE IF GENERO-P = "O"
                   MOVE TGEN(6) TO GEN-P

                READ CADCONV
                MOVE TPLANO(PLANO) TO PLAN
                IF ST-ERRO NOT = "00"
                   IF ST-ERRO = "23"
                    GO TO LER-CADPACI-FIM
                  ELSE
                    MOVE "*** ERRO GRAVE NA LEITURA CADCONV" TO MENS
                    PERFORM ROT-MENS THRU ROT-MENS-FIM
                    MOVE "ERRO" TO PLAN
                ELSE
                IF ST-ERRO NOT = "00"
                   IF ST-ERRO = "23"
                    IF W-SEL NOT = 1
                     MOVE "PACIENTE NAO CADASTRADO" TO NOME-P
                     MOVE "ERRO" TO SEX-P GEN-P PLAN
                     MOVE "*** CONVENIO NAO CADASTRADO" TO MENS
                     PERFORM ROT-MENS THRU ROT-MENS-FIM
                     DISPLAY TELACON
                    GO TO LER-CADPACI-FIM
                    ELSE
                     MOVE "PACIENTE NAO CADASTRADO" TO NOME-P
                     MOVE "ERRO" TO SEX-P GEN-P PLAN
                     MOVE "*** CONVENIO NAO CADASTRADO" TO MENS
                     PERFORM ROT-MENS THRU ROT-MENS-FIM
                     DISPLAY TELACON
                    GO TO LER-CADPACI-FIM
                  ELSE
                    MOVE "*** ERRO GRAVE NA LEITURA CADPACI" TO MENS
                    PERFORM ROT-MENS THRU ROT-MENS-FIM
                ELSE
                    DISPLAY TELACON.
       LER-CADPACI-FIM.

       INC-003.
                ACCEPT T-CRM
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-002C.
                IF CRM-MED = ZEROS 
                      MOVE "CRM NAO PODE SER ZERO"   TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-003.
                      
       INC-003A.
                MOVE CRM-MED TO CRM.
                READ CADMED
                IF SEXO-M = "M"
                   MOVE TSEX(1) TO SEX-M
                ELSE
                   MOVE TSEX(2) TO SEX-M
                MOVE TESP(ESPECIALIDADE) TO ESPEC
                IF ST-ERRO NOT = "00"
                   IF ST-ERRO = "23"
                    IF W-SEL NOT = 1
                     MOVE "MEDICO NAO CADASTRADO" TO NOME-M
                     MOVE "ERRO" TO SEX-M ESPEC
                     MOVE "*** MEDICO NAO CADASTRADO" TO MENS
                     PERFORM ROT-MENS THRU ROT-MENS-FIM
                     DISPLAY TELACON
                     GO TO INC-003A-FIM
                    ELSE
                     MOVE "MEDICO NAO CADASTRADO" TO NOME-M
                     MOVE "ERRO" TO SEX-M ESPEC
                     MOVE "*** MEDICO NAO CADASTRADO" TO MENS
                     PERFORM ROT-MENS THRU ROT-MENS-FIM
                     DISPLAY TELACON
                     GO TO INC-003A-FIM
                  ELSE
                    MOVE "*** ERRO GRAVE NA LEITURA CADMED" TO MENS
                    PERFORM ROT-MENS THRU ROT-MENS-FIM
                    GO TO ROT-FIM
                ELSE
                    DISPLAY TELACON.
       INC-003A-FIM.

       INC-004.
                ACCEPT T-CID
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-003.
                IF CID = ZEROS
                      MOVE "CID NAO PODE SER ZERO"   TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-004.
       INC-004A.
                MOVE CID TO CODCID.
                READ CADCID
                IF ST-ERRO NOT = "00"
                   IF ST-ERRO = "23"
                    IF W-SEL NOT = 1
                     MOVE "CID NAO CADASTRADO" TO DENOMINACAO
                     MOVE "*** CID NAO CADASTRADO" TO MENS
                     PERFORM ROT-MENS THRU ROT-MENS-FIM
                     DISPLAY TELACON
                     GO TO INC-004A-FIM
                    ELSE
                     MOVE "CID NAO CADASTRADO" TO DENOMINACAO
                     MOVE "*** CID NAO CADASTRADO" TO MENS
                     PERFORM ROT-MENS THRU ROT-MENS-FIM
                     DISPLAY TELACON
                     GO TO INC-004A-FIM
                  ELSE
                    MOVE "*** ERRO GRAVE NA LEITURA CADCID" TO MENS
                    PERFORM ROT-MENS THRU ROT-MENS-FIM
                ELSE
                    DISPLAY TELACON.
       INC-004A-FIM.

       INC-005.
                ACCEPT T-DESCR1
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT= 02
                    DISPLAY TELACON
                    GO TO INC-004.
                IF DESCRCON1 = SPACES
                      MOVE "DESCRICAO1 NAO PODE SER VAZIA"   TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-005.
       INC-006.
                ACCEPT T-DESCR2
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT= 02
                    DISPLAY TELACON
                    GO TO INC-005.
                IF DESCRCON2 = SPACES
                      MOVE "DESCRICAO2 NAO PODE SER VAZIA"   TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-006.

       INC-007.
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 
                    GO TO INC-006.

      *
                IF W-SEL = 1
                              GO TO ALT-OPC.

       INC-OPC.
                MOVE "S" TO W-OPCAO
                DISPLAY (23, 40) "DADOS OK (S/N) : ".
                ACCEPT (23, 57) W-OPCAO WITH UPDATE
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-007.
                IF W-OPCAO = "N" OR "n"
                   MOVE "* DADOS RECUSADOS PELO OPERADOR *" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM e N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-OPC.
       INC-WR1.
                WRITE REGCONSU
                IF ST-ERRO = "00" OR "02"
                      MOVE "*** DADOS GRAVADOS *** " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-001.
                IF ST-ERRO = "22"
                      MOVE "*** CONSULTA JA EXISTE ***       " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-001
                ELSE
                      MOVE ST-ERRO TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      MOVE "ERRO NA GRAVACAO DO ARQUIVO DE CONSULTAS"
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
                DELETE CADCONSU RECORD
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
                REWRITE REGCONSU
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
                CLOSE CADCONSU CADMED CADCID CADCONV CADPACI
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