       IDENTIFICATION DIVISION.
       PROGRAM-ID. SMP004.
       AUTHOR. JULIO CESAR DA SILVA BARCELLOS.
      **************************************
      * MANUTENCAO DO CADASTRO DE PACIENTES  *
      **************************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                         DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CADPACI ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS CPF
                    FILE STATUS  IS ST-ERRO
                    ALTERNATE RECORD IS NOMEP WITH DUPLICATES.

      *
      *-----------------------------------------------------------------
           SELECT CADCEP ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS CEP
                    FILE STATUS  IS ST-ERRO.
      *
      *-----------------------------------------------------------------
           SELECT CADCONV ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS CODIGO
                    FILE STATUS  IS ST-ERRO.
      *
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD CADPACI
            LABEL RECORD IS STANDARD
            VALUE OF FILE-ID IS "CADPACI.DAT".
       01 REGPACI.
                03 CPF             PIC 9(11).
                03 NOMEP           PIC X(30).
                03 DATANASC.
                    05 DIA         PIC 9(02).
                    05 MES         PIC 9(02).
                    05 ANO         PIC 9(04).
                03 SEXO            PIC X(01).
                03 GENERO          PIC X(01).
                03 CONVENIO        PIC 9(04).
                03 LOGRADOURO.
                    05 ACEP        PIC 9(08).
                    05 NUMCASA     PIC 9(04).
                    05 COMPLEMENTO PIC X(10).
                03 TELEFONE.
                        07 DDD     PIC 9(02).
                        07 NUMTEL  PIC 9(09).
                03 EMAIL           PIC X(30).

       FD CADCEP
            LABEL RECORD IS STANDARD
            VALUE OF FILE-ID IS "CADCEP.DAT".
       01 REGCEP.
                03 CEP             PIC 9(08).
                03 ENDERECO        PIC X(30).
                03 BAIRRO          PIC X(20).
                03 CIDADE          PIC X(20).
                03 UF              PIC X(02).
                
       FD CADCONV
            LABEL RECORD IS STANDARD
            VALUE OF FILE-ID IS "CADCONV.DAT".
       01 REGCONV.
                03 CODIGO          PIC 9(04).
                03 NOME            PIC X(30).
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
       77 GEN        PIC X(15) VALUE SPACES.
       77 SEX        PIC X(09) VALUE SPACES.
       77 PLAN       PIC X(25) VALUE SPACES.
       77 IND1       PIC 9(02) VALUE ZEROS.
       77 IND2       PIC 9(02) VALUE ZEROS.
       77 FLAG1      PIC 9(02) VALUE ZEROS.
       77 ANORESTO   PIC 9(04) VALUE ZEROS.
       77 ANODIV     PIC 9(05) VALUE ZEROS.

       01 TABUNIDADE01.
          03 T01     PIC X(06) VALUE "NHBTPO".
       01 TUN1 REDEFINES TABUNIDADE01.
          03 T1      PIC X(01) OCCURS 6 TIMES.
       01 ALFAUN     PIC 9(01) VALUE ZEROS.
       01 TABGEN.
          03 GEN1    PIC X(15) VALUE "NAO DECLARADO".
          03 GEN2    PIC X(15) VALUE "HETEROSEXUAL".
          03 GEN3    PIC X(15) VALUE "BISEXUAL".
          03 GEN4    PIC X(15) VALUE "TRANSEXUAL".
          03 GEN5    PIC X(15) VALUE "PANSEXUAL".
          03 GEN6    PIC X(15) VALUE "OUTROS".
       01 TUN01 REDEFINES TABGEN.
           03 TGEN     PIC X(15) OCCURS 06 TIMES.

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
      *
      *-------------------[ SECAO DE TELA ]------------------------ 
       SCREEN SECTION.
       01  TELAPAC.
            05  BLANK SCREEN.
           05  LINE 02  COLUMN 01 
               VALUE  "                            CADASTRO DE".
           05  LINE 02  COLUMN 41 
               VALUE  "PACIENTE".
           05  LINE 04  COLUMN 01 
               VALUE  " CPF:              NOME:".
           05  LINE 06  COLUMN 01 
               VALUE  " DT. NASCIMENTO:   /  /      SEXO:".
           05  LINE 06  COLUMN 41 
               VALUE  "        GENERO:".
           05  LINE 08  COLUMN 01 
               VALUE  " CONVENIO:".
           05  LINE 08  COLUMN 41 
               VALUE  "       PLANO:".
           05  LINE 10  COLUMN 01 
               VALUE  " ENDERECO -> CEP:           LOGRADOURO:".
           05  LINE 12  COLUMN 01 
               VALUE  " NUMERO:      COMPLEMENTO:            BA".
           05  LINE 12  COLUMN 41 
               VALUE  "IRRO:".
           05  LINE 14  COLUMN 01 
               VALUE  " CIDADE:                      UF:    TEL".
           05  LINE 14  COLUMN 41 
               VALUE  "EFONE: (  )".
           05  LINE 16  COLUMN 01 
               VALUE  " EMAIL:".
           05  LINE 23  COLUMN 01 
               VALUE  " MENSAGEM".
           05  T-CPF
               LINE 04  COLUMN 07  PIC 9(11)
               USING  CPF.
           05  T-NOMEP
               LINE 04  COLUMN 26  PIC X(30)
               USING  NOMEP.
           05  T-DIA
               LINE 06  COLUMN 18  PIC 9(02)
               USING  DIA.
           05  T-MES
               LINE 06  COLUMN 21  PIC 9(02)
               USING  MES.
           05  T-ANO
               LINE 06  COLUMN 24  PIC 9(04)
               USING  ANO.
           05  T-SEXO
               LINE 06  COLUMN 36  PIC X(01)
               USING  SEXO.
           05  T-SEX
               LINE 06  COLUMN 38  PIC X(09)
               USING  SEX.
           05  T-GENERO
               LINE 06  COLUMN 57  PIC X(01)
               USING  GENERO.
           05  T-GEN
               LINE 06  COLUMN 59  PIC X(15)
               USING  GEN.
           05  T-CONVENIO
               LINE 08  COLUMN 12  PIC 9(04)
               USING  CONVENIO.
           05  T-NCONV
               LINE 08  COLUMN 17  PIC X(30)
               USING  NOME.
           05  T-PLAN
               LINE 08  COLUMN 55  PIC X(25)
               USING  PLAN.
           05  T-CEP
               LINE 10  COLUMN 19  PIC 9(08)
               USING  ACEP.
           05  T-ENDERECO
               LINE 10  COLUMN 41  PIC X(30)
               USING  ENDERECO.
           05  T-NUMCASA
               LINE 12  COLUMN 10  PIC 9(04)
               USING  NUMCASA.
           05  T-COMPL
               LINE 12  COLUMN 28  PIC X(10)
               USING  COMPLEMENTO.
           05  T-BAIRRO
               LINE 12  COLUMN 47  PIC X(20)
               USING  BAIRRO.
           05  T-CIDADE
               LINE 14  COLUMN 10  PIC X(20)
               USING  CIDADE.
           05  T-UF
               LINE 14  COLUMN 35  PIC X(02)
               USING  UF.
           05  T-DDD
               LINE 14  COLUMN 49  PIC 9(02)
               USING  DDD.
           05  T-NUMTEL
               LINE 14  COLUMN 53  PIC 9(09)
               USING  NUMTEL.
           05  T-EMAIL
               LINE 16  COLUMN 09  PIC X(30)
               USING  EMAIL.
           05  TMENS
               LINE 23  COLUMN 12  PIC X(50)
               USING  MENS.

      *
       01  TELAGEN.
           05  FOREGROUND-COLOR  1  BACKGROUND-COLOR 0.
           05  LINE 18  COLUMN 01 
               VALUE  "                 TABELA DE GENEROS (USE ".
           05  LINE 18  COLUMN 41 
               VALUE  "ESC PARA NAVEGAR)".
           05  LINE 20  COLUMN 01 
               VALUE  "  N - NAO DECLARADO   H - HETEROSEXUAL ".
           05  LINE 20  COLUMN 41 
               VALUE  " B - BISEXUAL   T - TRANSEXUAL".
           05  LINE 21  COLUMN 01 
               VALUE  "                     P - PANSEXUAL   O -".
           05  LINE 21  COLUMN 41 
               VALUE  " OUTROS".

      *
       01 TELASEXO.
           05  FOREGROUND-COLOR  1  BACKGROUND-COLOR 0.
            05  LINE 18  COLUMN 01 
               VALUE  "                 TABELA DE SEXO (USE ESC".
           05  LINE 18  COLUMN 41 
               VALUE  " PARA NAVEGAR)".
           05  LINE 20  COLUMN 01 
               VALUE  "                     M - MASCULINO    F".
           05  LINE 20  COLUMN 41 
               VALUE  "- FEMININO".
      * 
      *-------------------[ DIVISAO DE PROCEDIMENTOS ]------------------
       PROCEDURE DIVISION.
       INICIO.
      *
       INC-OP0.
           OPEN I-O CADPACI
           IF ST-ERRO NOT = "00"
               IF ST-ERRO = "30"
                      OPEN OUTPUT CADPACI
                      CLOSE CADPACI
                      MOVE "* ARQUIVO CADPACI SENDO CRIADO *" 
                      TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-OP0
               ELSE
                      MOVE "ERRO NA ABERTURA DO ARQUIVO CADPACI" 
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
                    
       INC-OP2.
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

       INC-001.
                MOVE ZEROS  TO CPF ACEP CEP NUMCASA
                               TELEFONE GENERO CONVENIO
                               DATANASC IND1 IND2.
                MOVE SPACES TO NOME NOMEP COMPLEMENTO
                               EMAIL SEXO GEN SEX
                               ENDERECO BAIRRO CIDADE UF.
                DISPLAY TELAPAC.
       INC-002.
                ACCEPT  T-CPF
                ACCEPT W-ACT FROM ESCAPE KEY
                 IF W-ACT = 02
                   CLOSE CADPACI
                   GO TO ROT-FIM.
                IF CPF  = ZEROS
                   MOVE "*** CPF NAO PODE SER ZERO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-002.
                MOVE 1 TO IND1
                MOVE 1 TO IND2
                MOVE T1(IND1) TO GENERO.
                MOVE TGEN(IND1) TO GEN.
                MOVE T2(IND2) TO SEXO.
                MOVE TSEX(IND2) TO SEX.
                DISPLAY T-GENERO T-GEN T-SEXO T-SEX.
       LER-CADMED01.
                MOVE 0 TO W-SEL
                READ CADPACI
                IF ST-ERRO NOT = "23"
                   IF ST-ERRO = "00"
                      MOVE 1 TO W-SEL
                      PERFORM INC-007A THRU INC-007B
                      PERFORM INC-008A THRU INC-008B
                      PERFORM INC-009A THRU INC-009-FIM
                      PERFORM INC-010A THRU INC-010-FIM
                      DISPLAY TELAPAC
                      MOVE "*** PACIENTE JA CADASTRADO ***" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      MOVE 1 TO W-SEL
                      GO TO ACE-001
                   ELSE
                      MOVE "ERRO NA LEITURA DO ARQUIVO CADPACI" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                ELSE
                   NEXT SENTENCE.
       INC-003.
                ACCEPT T-NOMEP
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-002.
                IF NOMEP = SPACES 
                      MOVE "NOME NAO PODE SER BRANCO"   TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-003.
                      
       INC-004.
                ACCEPT T-DIA
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-003.
                MOVE 0 TO FLAG1.
                IF DIA = ZEROS
                      MOVE "DIA NAO PODE SER ZERO"   TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-004.
                MOVE SPACES TO MENS.
                DISPLAY TMENS.
       INC-005.
                ACCEPT T-MES
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-004.
                MOVE 0 TO FLAG1.
                IF MES = ZEROS
                      MOVE "MES NAO PODE SER ZERO"   TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-005.
       INC-006.
                ACCEPT T-ANO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-005.
                IF ANO = ZEROS
                      MOVE "ANO NAO PODE SER ZERO"   TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-006.
                MOVE 0 TO FLAG1.
      *               Preciso Verificar a lógica desse Flag acima
                PERFORM VALIDADATA THRU VALIDADATA-FIM
                IF FLAG1 NOT = 0
                    IF FLAG1 = 1
                       GO TO INC-005.
                    IF FLAG1 = 2
                       GO TO INC-004.
                DISPLAY TELAPAC.
                
       INC-007.
                DISPLAY TELASEXO
                ACCEPT T-SEXO
                MOVE TSEX(IND2) TO SEX.
                DISPLAY T-SEXO T-SEX
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02
                    DISPLAY TELAPAC
                    GO TO INC-006.
                IF W-ACT = 01 
                       IF IND2 = 2
                              MOVE 1 TO IND2
                              MOVE T2(IND2) TO SEXO
                              MOVE TSEX(IND2) TO SEX
                              DISPLAY T-SEXO T-SEX
                              GO TO INC-007
                       ELSE
                              ADD 1 TO IND2
                              MOVE T2(IND2) TO SEXO
                              MOVE TSEX(IND2) TO SEX
                              DISPLAY T-SEXO T-SEX
                              GO TO INC-007.
                IF SEXO = SPACES                      
                      MOVE "SEXO NAO PODE SER BRANCO"   TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-007.
       INC-007A.
             MOVE 1 TO IND2.
       INC-007B.
            MOVE T2(IND2) TO ALFAUN2
            IF SEXO  NOT = ALFAUN2
                    IF IND2 < 2
                             ADD 1 TO IND2
                             GO TO INC-007B
                     ELSE
                        IF W-SEL = 0
                             MOVE "SEXO INVALIDO" TO MENS
                             PERFORM ROT-MENS THRU ROT-MENS-FIM
                             GO TO INC-007
                         ELSE
                             MOVE  ALL "-" TO SEX
             ELSE
                      MOVE TSEX(IND2) TO SEX.
              DISPLAY TELASEXO.
                  
       INC-008.
                DISPLAY TELAGEN
                ACCEPT T-GENERO
                MOVE TGEN(IND1) TO GEN.
                DISPLAY T-GENERO T-GEN
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 
                    DISPLAY TELAPAC TELASEXO
                    GO TO INC-007.
                IF W-ACT = 01 
                       IF IND1 = 06
                              GO TO INC-100
                       ELSE
                              ADD 1 TO IND1
                              GO TO INC-101.
                IF GENERO = ZEROS
                      MOVE "GENERO NAO PODE SER VAZIO"   TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-008.
                GO TO INC-008A.
        INC-100.
            MOVE 1 TO IND1
            MOVE T1(IND1) TO GENERO
            MOVE TGEN(IND1) TO GEN
            DISPLAY T-GENERO T-GEN
            GO TO INC-008.
        INC-101.
            MOVE T1(IND1) TO GENERO
            MOVE TGEN(IND1) TO GEN
            DISPLAY T-GENERO T-GEN
            GO TO INC-008.
       INC-008A.
             MOVE 1 TO IND1.
       INC-008B.
            MOVE T1(IND1) TO ALFAUN
            IF GENERO  NOT = ALFAUN
                    IF IND1 < 6
                             ADD 1 TO IND1
                             GO TO INC-008B
                     ELSE
                        IF W-SEL = 0
                             MOVE "GENERO INVALIDO" TO MENS
                             PERFORM ROT-MENS THRU ROT-MENS-FIM
                             GO TO INC-008
                         ELSE
                             MOVE  ALL "-" TO GEN
             ELSE
                      MOVE TGEN(IND1) TO GEN.
              DISPLAY TELAGEN.

       INC-009.
                ACCEPT T-CONVENIO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT= 02
                    DISPLAY TELAPAC TELAGEN
                    GO TO INC-008.
                IF CONVENIO = ZEROS
                      MOVE "CONVENIO NAO PODE SER ZERO"   TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-009.
       INC-009A.
                MOVE CONVENIO TO CODIGO.
                READ CADCONV
                MOVE TPLANO(PLANO) TO PLAN
                IF ST-ERRO NOT = "00"
                   IF ST-ERRO = "23"
                    IF W-SEL NOT = 1
                     MOVE "CONVENIO NAO CADASTRADO" TO NOME
                     MOVE "CONVENIO NAO CADASTRADO" TO PLAN
                     MOVE "*** CONVENIO NAO CADASTRADO" TO MENS
                     PERFORM ROT-MENS THRU ROT-MENS-FIM
                     DISPLAY TELAPAC
                     GO TO INC-010
                    ELSE
                     MOVE "CONVENIO NAO CADASTRADO" TO NOME
                     MOVE "CONVENIO NAO CADASTRADO" TO PLAN
                     MOVE "*** CONVENIO NAO CADASTRADO" TO MENS
                     PERFORM ROT-MENS THRU ROT-MENS-FIM
                     DISPLAY TELAPAC
                     GO TO INC-009-FIM
                  ELSE
                    MOVE "*** ERRO GRAVE NA LEITURA CADCONV" TO MENS
                    PERFORM ROT-MENS THRU ROT-MENS-FIM
                    GO TO ROT-FIM
                ELSE
                    DISPLAY TELAPAC.
       INC-009-FIM.	

       INC-010.
                ACCEPT T-CEP
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT= 02 GO TO INC-009.
                IF ACEP = ZEROS
                      MOVE "CEP NAO PODE SER ZERO"   TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-010.
       INC-010A.
                MOVE ACEP TO CEP.
                READ CADCEP
                IF ST-ERRO NOT = "00"
                   IF ST-ERRO = "23"
                    IF W-SEL NOT = 1
                     MOVE "CEP NAO CADASTRADO" TO ENDERECO BAIRRO
                     MOVE "CEP NAO CADASTRADO" TO CIDADE
                     MOVE "*** CEP NAO CADASTRADO" TO MENS
                     PERFORM ROT-MENS THRU ROT-MENS-FIM
                     DISPLAY TELAPAC
                     GO TO INC-011
                    ELSE
                     MOVE "CEP NAO CADASTRADO" TO ENDERECO BAIRRO
                     MOVE "CEP NAO CADASTRADO" TO CIDADE
                     MOVE "*** CEP NAO CADASTRADO" TO MENS
                     PERFORM ROT-MENS THRU ROT-MENS-FIM
                     DISPLAY TELAPAC
                     GO TO INC-010-FIM
                  ELSE
                    MOVE "*** ERRO GRAVE NA LEITURA CADCEP" TO MENS
                    PERFORM ROT-MENS THRU ROT-MENS-FIM
                    GO TO ROT-FIM
                ELSE
                    DISPLAY TELAPAC.
       INC-010-FIM.	

       INC-011.
                ACCEPT T-NUMCASA
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT= 02 GO TO INC-010.
                IF NUMCASA = ZEROS
                      MOVE "NUMERO NAO PODE SER ZERO"   TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-011.

       INC-012.
                ACCEPT T-COMPL
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT= 02 GO TO INC-011.

       INC-013.
                ACCEPT T-DDD
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT= 02 GO TO INC-012.
                IF DDD = ZEROS
                      MOVE "DDD NAO PODE SER ZERO"   TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-013.

       INC-013A.
                ACCEPT T-NUMTEL
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT= 02 GO TO INC-013.
                IF NUMTEL = ZEROS
                      MOVE "TELEFONE NAO PODE SER ZERO"   TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-013A.

       INC-014.
                ACCEPT T-EMAIL
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT= 02 GO TO INC-013A.
                IF EMAIL = SPACES
                      MOVE "EMAIL NAO PODE SER VAZIO"   TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-014.

       INC-015.
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 
                    GO TO INC-014.

      *
                IF W-SEL = 1
                              GO TO ALT-OPC.

       INC-OPC.
                MOVE "S" TO W-OPCAO
                DISPLAY (23, 40) "DADOS OK (S/N) : ".
                ACCEPT (23, 57) W-OPCAO WITH UPDATE
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-015.
                IF W-OPCAO = "N" OR "n"
                   MOVE "* DADOS RECUSADOS PELO OPERADOR *" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM e N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-OPC.
       INC-WR1.
                WRITE REGPACI
                IF ST-ERRO = "00" OR "02"
                      MOVE "*** DADOS GRAVADOS *** " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-001.
                IF ST-ERRO = "22"
                      MOVE "*** PACIENTE JA EXISTE ***       " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-001
                 ELSE
                      MOVE ST-ERRO TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      MOVE "ERRO NA GRAVACAO DO ARQUIVO DE PACIENTES"
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
                DELETE CADPACI RECORD
                IF ST-ERRO = "00"
                   MOVE "*** REGISTRO EXCLUIDO ***        " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001
                ELSE
                   MOVE ST-ERRO TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
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
                REWRITE REGPACI
                IF ST-ERRO = "00" OR "02"
                   MOVE "*** REGISTRO ALTERADO ***         " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001
                ELSE
                   MOVE ST-ERRO TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   MOVE "ERRO NA EXCLUSAO DO REGISTRO PRODUTO"   TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ROT-FIM.
      *
      **********************
      * ROTINA DE FIM      *
      **********************
      *
       ROT-FIM.
                CLOSE CADPACI CADCEP CADCONV
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