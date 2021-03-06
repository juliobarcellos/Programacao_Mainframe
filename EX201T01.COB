       IDENTIFICATION DIVISION.
       PROGRAM-ID. EX201T01.
       AUTHOR. JULIO CESAR DA SILVA BARCELLOS.
      **************************************
      * MANUTENCAO DO CADASTRO DE ALUNOS  *
      **************************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                         DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT CADALUNO ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS RA
                    FILE STATUS  IS ST-ERRO
                    ALTERNATE RECORD KEY IS NOME  WITH DUPLICATES.
      *
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD CADALUNO
            LABEL RECORD IS STANDARD
            VALUE OF FILE-ID IS "CADALUNO.DAT".
       01 REGALUNO.
                03 RA              PIC 9(06).
                03 NOME            PIC X(30).
                03 CPF             PIC 9(11).
                03 RG              PIC X(12).
                03 DATANASCIMENTO.
                    05 DIA         PIC 9(02).
                    05 MES         PIC 9(02).
                    05 ANO         PIC 9(04).
                03 NATURAL         PIC X(20).
                03 NACIONALIDADE   PIC X(20).
                03 ENDERECO.
                    05 LOGRADOURO  PIC X(30).
                    05 NUMERO      PIC 9(05).
                    05 COMPLEMENTO PIC X(20).
                    05 BAIRRO      PIC X(20).
                    05 CIDADE      PIC X(25).
                    05 ESTADO      PIC X(02).
                    05 CEP         PIC 9(08).
                03 EMAIL           PIC X(30).
                03 TELEFONE.
                    05 DDD         PIC 9(02).
                    05 NUMEROT     PIC 9(09).
      *
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
       77 W-SEL        PIC 9(01) VALUE ZEROS.
       77 W-CONT       PIC 9(06) VALUE ZEROS.
       77 W-OPCAO      PIC X(01) VALUE SPACES.
       77 ST-ERRO      PIC X(02) VALUE "00".
       77 W-ACT        PIC 9(02) VALUE ZEROS.
       77 MENS         PIC X(50) VALUE SPACES.
       77 LIMPA        PIC X(50) VALUE SPACES.
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO.
      *
       INC-OP0.
           OPEN I-O CADALUNO
           IF ST-ERRO NOT = "00"
               IF ST-ERRO = "30"
                      OPEN OUTPUT CADALUNO
                      CLOSE CADALUNO
                      MOVE "* ARQUIVO CADALUNO SENDO CRIADO *" 
                      TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-OP0
                   ELSE
                      MOVE "ERRO NA ABERTURA DO ARQUIVO CADALUNO" 
                      TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                ELSE
                    NEXT SENTENCE.
       INC-001.
                MOVE ZEROS  TO RA CPF
                               TELEFONE
                MOVE SPACES TO NOME RG DATANASCIMENTO
                                NATURAL NACIONALIDADE
                                ENDERECO EMAIL.
                DISPLAY (01, 01) ERASE.
                DISPLAY (01, 20) "CADASTRO DE ALUNOS"
                DISPLAY (04, 01) "RA              : "
                DISPLAY (05, 01) "NOME            : "
                DISPLAY (06, 01) "CPF             : "
                DISPLAY (07, 01) "RG              : "
                DISPLAY (08, 01) "DATA NASCIMENTO : "
                DISPLAY (09, 01) "NATURAL         : "
                DISPLAY (10, 01) "NACIONALIDADE   : "
                DISPLAY (11, 01) "ENDERECO        : "
                DISPLAY (12, 01) "LOGRADOURO      : "
                DISPLAY (13, 01) "NUMERO          : "
                DISPLAY (13, 27) "COMPLEMENTO : "
                DISPLAY (14, 01) "BAIRRO          : "
                DISPLAY (15, 01) "CIDADE          : "
                DISPLAY (16, 01) "ESTADO          : "
                DISPLAY (16, 27) "CEP : "
                DISPLAY (17, 01) "E-MAIL          : "
                DISPLAY (18, 01) "TELEFONE        : "
                DISPLAY (08, 21) "/"
                DISPLAY (08, 24) "/"
                DISPLAY (18, 19) "("
                DISPLAY (18, 22) ")".
       INC-002.
                ACCEPT  (04, 19) RA
                ACCEPT W-ACT FROM ESCAPE KEY
                 IF W-ACT = 02
                   CLOSE CADALUNO
                   GO TO ROT-FIM.
                IF RA  = 0
                   MOVE "*** RA INVALIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-002.
       LER-CADALUNO01.
                MOVE 0 TO W-SEL
                READ CADALUNO
                IF ST-ERRO NOT = "23"
                   IF ST-ERRO = "00"
                      DISPLAY (05, 19) NOME
                      DISPLAY (06, 19) CPF
                      DISPLAY (07, 19) RG
                      DISPLAY (08, 19) DIA
                      DISPLAY (08, 22) MES
                      DISPLAY (08, 25) ANO
                      DISPLAY (09, 19) NATURAL
                      DISPLAY (10, 19) NACIONALIDADE
                      DISPLAY (12, 19) LOGRADOURO
                      DISPLAY (13, 19) NUMERO
                      DISPLAY (13, 41) COMPLEMENTO
                      DISPLAY (14, 19) BAIRRO
                      DISPLAY (15, 19) CIDADE
                      DISPLAY (16, 19) ESTADO
                      DISPLAY (16, 33) CEP
                      DISPLAY (17, 19) EMAIL
                      DISPLAY (18, 20) DDD
                      DISPLAY (18, 24) NUMEROT
                      DISPLAY (08, 21) "/"
                      DISPLAY (08, 24) "/"
                      DISPLAY (18, 19) "("
                      DISPLAY (18, 22) ")"
                      
                      MOVE "*** ALUNO JA CADASTRADO ***" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      MOVE 1 TO W-SEL
                      GO TO ACE-001
                   ELSE
                      MOVE "ERRO NA LEITURA DO ARQUIVO CADALUNO" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                ELSE
                   NEXT SENTENCE.
       INC-003.
                ACCEPT (05, 19) NOME
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-002.
       INC-004.
                ACCEPT (06, 19) CPF
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-003.
       INC-005.
                ACCEPT (07, 19) RG
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-004.
       INC-006.
                ACCEPT (08, 19) DIA
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-005.
       INC-007.
                ACCEPT (08, 22) MES
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-006.
       INC-008.
                ACCEPT (08, 25) ANO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-007.
       INC-009.
                ACCEPT (09, 19) NATURAL
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-008.
       INC-010.
                ACCEPT (10, 19) NACIONALIDADE
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-009.
       INC-011.
                ACCEPT (12, 19) LOGRADOURO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-010.
       INC-012.
                ACCEPT (13, 19) NUMERO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-011.
       INC-013.
                ACCEPT (13, 41) COMPLEMENTO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-012.
       INC-014.
                ACCEPT (14, 19) BAIRRO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-013.
       INC-015.
                ACCEPT (15, 19) CIDADE
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-014.
       INC-016.
                ACCEPT (16, 19) ESTADO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-015.
       INC-017.
                ACCEPT (16, 33) CEP
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-016.
       INC-018.
                ACCEPT (17, 19) EMAIL
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-017.
       INC-019.
                ACCEPT (18, 20) DDD
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-018.
       INC-020.
                ACCEPT (18, 24) NUMEROT
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-019.
      *
                IF W-SEL = 1
                              GO TO ALT-OPC.

       INC-OPC.
                MOVE "S" TO W-OPCAO
                DISPLAY (23, 40) "DADOS OK (S/N) : ".
                ACCEPT (23, 57) W-OPCAO WITH UPDATE
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-020.
                IF W-OPCAO = "N" OR "n"
                   MOVE "* DADOS RECUSADOS PELO OPERADOR *" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM e N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-OPC.
       INC-WR1.
                WRITE REGALUNO
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
                DELETE CADALUNO RECORD
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
                REWRITE REGALUNO
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
               DISPLAY (23, 12) MENS.
       ROT-MENS2.
                ADD 1 TO W-CONT
                IF W-CONT < 300
                   GO TO ROT-MENS2
                ELSE
                   DISPLAY (23, 12) LIMPA.
       ROT-MENS-FIM.
                EXIT.
       FIM-ROT-TEMPO.
