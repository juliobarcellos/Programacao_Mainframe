# Programação Para Mainframe

Repositório para o conteúdo das aulas e exercícios propostos na matéria cursada no 4º semestre do curso de análise e desenvolvimento de sistemas.

## Configuração do projeto

Todas as aulas e exercícios foram feitos utilizando as seguintes ferramentas:
* [DOSBox](https://www.dosbox.com/) - O compilador funciona apenas em 32bits então foi necessário usar o emulador DosBox para rodar corretamente;
* um editor de texto de sua preferência (utilizamos o [Notepad++](https://notepad-plus-plus.org/downloads/) pois auxilia no entendimento dos comandos da linguagem;
* o [Compilador](https://github.com/juliobarcellos/Programacao_Mainframe/tree/main/COBOL) se encontra na pasta COBOL. Utilizamos apenas esta forma de trabalho durante o curso.


### Build and Test

* Para rodar qualquer programa é só copiar os arquivos `.COB` para dentro da pasta COBOL e fazer os passos abaixo:
  - Arrastar a pasta COBOL para o ícone do executável do DOSBox, ou montar o diretório da pasta COBOL no DOSBox (Exemplo: Mount Z "C://COBOL");
  - Executar o comando `ISAM` no DOSBox para iniciar o programa que acessa os arquivos de registro (Index Sequential Access Method);
  - Executar o comando "Cobol + NomedoPrograma+,,," (Exemplo: `Cobol CEP001,,,`) para executar o compilador, ele deve exibir a tela abaixo com a informação "No errors or warnings");
  - ![Imagem exibindo a tela do compilador de Cobol exibindo o processo e a mensagem "4 errors or warnings"](https://i.stack.imgur.com/4Vcf1.png)
  - Executar o comando "Runcob + NomedoPrograma (Exemplo: `Runcob CEP001`);

* Dicas:
  - Você pode usar F1 para voltar um campo já digitado e digitar novamente;
  - Você pode usar F2 para tentar avançar um campo (caso o campo não possa ser vazio vai dar erro);
  - Em alguns momentos serão exibidas diversas opções pré definidas para preencher alguns campos, você pode digitar o código ou apertar a tecla ESC para ir navegando entre as opções.

* OBS: Em todos os programas que existe registro e validação de datas a validação ainda está com erro, não consegui fazer rodar corretamente.
  - Para o [Sistema_Medico_Paciente](https://github.com/juliobarcellos/Programacao_Mainframe/tree/main/Sistema_Medico_Paciente) eu mantive os arquivos de registro e chaves no GitHub para que o sistema possa ser testado sem que seja necessária a digitação de valores. Detalhamento das chaves cadastradas estão no exercício.
  - No [EXERCICIO 6](Sistema_Medico_Paciente/SMP006.COB) do Sistema Médico Paciente não consegui resolver um erro que não deixa o programa rodar, dizendo que tem algum valor não numérico sendo inserido em um campo numérico



## Informações gerais

### [CEP001](CEP001.COB)
Programa de Cadastro de Produtos
- Foi o primeiro programa que trabalhamos, aprendendo conceitos básicos de cada division e os comandos básicos da linguagem.

### [EX201T01](EX201T01.COB)
Exercício valendo nota contendo programa de cadastro de alunos.
- Fazer um sistema de cadastro de alunos contendo os campos abaixo:
- Nome >   EX201T01         
         ARQUIVO   :   CADALUNO
         REGISTRO  :   REGALUNO

         CAMPOS :
         RA / RM           9(6)
         NOME             X(30)
         CPF              9(11)
         RG               X(12)
         DATA NASCIMENTO
              DIA         9(02)
              MES         9(02)
              ANO         9(04)
         NATURAL          X(20) 
         NACIONALIDADE    X(20)
         ENDERECO
              LOGRADOURO  X(30)
              NUMERO      9(3)
              COMPLEMENTO X(12)
              BAIRRO      X(20)
              CIDADE      X(20)
              ESTADO      X(2)
              CEP         9(8)
          EMAIL           X(30)
          TELEFONE             
              DDD         9(2)
              NUMERO      9(9)
 
### [PROGR02T](PROGR02T.COB)
Exercício valendo nota contendo programa de cadastro de Funcionários. Aprendemos a usar a SCREEN SECTION
- Fazer um sistema de cadastro de funcionários contendo os campos e tabelas abaixo:
-  Nome >   PROGR02T         
         ARQUIVO   :  CADFUN
         REGISTRO  :  REGFUN 

        CAMPOS :
        NUMERO REGISTRO   9(6)
        NOME              X(30)                          
        DEPARTAMENTO      9(1)  (usar a TABELA DEPARTAMENTOS)
        CARGO             9(1)  (usar a TABELA DE CARGOS)
        SALARIO BASE      9(06)V99
        NUMERO FILHOS     9(1)
        DATA ADMISSÃO     9(8)       DDMMAAAA
        DATA DEMISÃO      9(8)       DDMMAAAA
        
        TABELA DEPARTAMENTOS
        1-COMERCIAL
        2-ADMINISTRATIVA
        3-FINANCEIRA
        4-RECURSOS HUMANOS                                        
        5-CONTABILIDADE                                                    
        6-TECNOLOGIA INFORMAÇÃO                              
        7-VENDAS                                                                  
        8-SERVICOS GERAIS                                                  
        9-TRANSPORTES                                                          

        TABELA DE CARGOS
        0-DIRETOR
        1-GERENTE
        2-SUPERVISOR
        3-COORDENADOR
        4-ASSISTENTE
        5-AUXILIAR
        6-CONTINUO
        7-VENDEDOR
        8-VIGIA
        9-TELEFONISTA
        
         OBSERVAÇÕES : DEVE SER UTILIZADO SCREEN SECTION;
                       TODOS OS CAMPOS DEVEM SER CONSISTIDOS;
                       AS DATAS DEVEM SER VALIDADAS;
                       OS CAMPOS DEPARTAMENTO E CARGO DEVEM CONTER NA FRENTE A DESCRIÇÃO DA TABELA.


### [Programa_Cadastro_Amigo](https://github.com/juliobarcellos/Programacao_Mainframe/tree/main/Programa_Cadastro_Amigo)
Exercício valendo nota contendo programa de cadastro de Amigos e de Endereço. Aprendemos como utilizar o `OPEN INPUT` para buscar registros de outros arquivos como somente leitura utilizando uma chave primária.

- Fazer um [sistema de cadastro de endereços](Programa_Cadastro_Amigo/PCADEND.COB) contendo os campos e tabelas abaixo:
- -  Nome >   PCADEND         
         ARQUIVO   :  ARQENDER
         REGISTRO  :  REGENDER 

        CAMPOS :
        CEP              9(08)
        ENDERECO         X(35)
        BAIRRO           X(20)
        CIDADE           X(35)
        UF               X(02)
        
        TABELA UF
        SP- "SAO PAULO"
        RJ- "RIO DE JANEIRO"
        PR- "PARANA"
        RS- "RIO GRANDE DO SUL"
        SC- "SANTA CATARINA"
        MG- "MINAS GERAIS"
        MT- "MATO GROSSO"  
        
- Fazer um [sistema de cadastro de amigos](Programa_Cadastro_Amigo/PCADAMG.COB) conectando com o cadastro de endereços e usando os campos e tabelas abaixo:
-  Nome >   PCADAMIG         
         ARQUIVO   :  ARQAMIGO
         REGISTRO  :  REGAMIGO 

        CAMPOS :
                APELIDO          X(12)
                NOME             X(35)
                LOGRADOURO
                  ACEP           9(08)
                  NUMERO         9(04)
                  COMPLEMENTO    X(12)
                TELEFONES
                  CELULAR
                    DDDCEL       9(03)
                    NUMCEL       9(09)
                  RESIDENCIAL
                    DDDRES       9(03)
                    NUMRES       9(08)
                EMAIL            X(35)
                TIPOAMIGO        9(01)
                DATANASC
                  DIA            9(02)
                  MES            9(02)
                  ANO            9(04)
                SEXO             X(01)
                OPCSEX           X(01)
        
        TABELA TIPO AMIGO
        1-BAIRRO
        2-FACULDADE
        3-COLEGIO
        4-CLUBE                                        
        5-BAIRRO                                                    
        6-IGREJA                            
        7-BALADA                                                                  
        8-EX NAMORADO(A)                                                 
        9-NAMORADO DO(A) EX                                                          

        TABELA DE SEXO
        M-MASCULINO
        F-FEMININO
        
        TABELA DE GENERO
        E-HETEROSEXUAL
        B-BISEXUAL
        T-TRANSSEXUAL
        P-PANSEXUAL
        H-HOMOSEXUAL
        N-NAO DECLARADO
        O-OUTROS
        
        Obs: O programa deve puxar o endereço usando o CEP como chave primária para trazer as informações do arquivo ARQENDER
        
### [Programa_Notas](https://github.com/juliobarcellos/Programacao_Mainframe/tree/main/Programa_Notas)
Aprendemos a utilizar aqui campos calculados 
- Fazer um [Sistema de Notas](Programa_Notas/PNOTA.COB) usando os campos e tabelas abaixo:
         NOME >   PNOTA         
         ARQUIVO   :  CADNOTA
         REGISTRO  :  REGNOTA 

        CAMPOS :
        MATRICULA        9(06)
        NOME             X(30)
        NOTA1            9(02)V9
        NOTA2            9(02)V9
        
        Obs: Incluir os campos com a média das duas notas.
 
 - Fazer um [Segundo sistema de Notas](Programa_Notas/PNOTA1.COB) incluíndo campos para calcular as faltas dos dois bimestres:
         NOME >   PNOTA1       
         ARQUIVO   :  CADNOTA1
         REGISTRO  :  REGNOTA 

        CAMPOS :
        MATRICULA        9(06)
        NOME             X(30)
        NOTA1            9(02)V9
        NOTA2            9(02)V9
        FALTA1           9(02)
        FALTA2           9(02)
        
        Obs: Incluir os campos com a média das duas notas e a soma das faltas.
 
### [Sistema_Medico_Paciente](https://github.com/juliobarcellos/Programacao_Mainframe/tree/main/Sistema_Medico_Paciente)
Trabalho valendo nota contendo um sistema de cadastro e manutenção de endereços, CID's, Convenios, Médicos, Pacientes e Consultas.

- OBS: Para esse sistema deixei no repositório os arquivos .DAT e .KEY para que não seja necessária a inserção de valores para testar o sistema. Logo abaixo estão os registros para utilização na consulta (Não existem registros de consultas pois o sistema não está funcional ainda):
          
          CEP
					11111111 Avenida Paulista
					22222222 Rua Aires Saldanha
					33333333 Rua Maringa
					44444444 Rua Minas Gerais
					
          CID
					1010 Peste
					1011 Leptospirose
					1012 Coqueluche
					1013 Febres Recorrentes
					1014 Dengue
					1015 Meningite Viral
					
					Convenio
					1110 Bradesco Seguros / Enfermaria Regional
					1111 Bradesco Seguros / Enfermaria Nacional
					1112 Bradesco Seguros / Enfermaria Internacional
					1113 Amil / Emergência Internacional
					1114 Amil / Emergência Nacional
					1115 Amil / Plano Global
					
					Paciente
					11111111111 Paciente 1
					22222222222 Paciente 2
					33333333333 Paciente 3
					44444444444 Paciente 4
          
          Médico
					111111 Medico 1
					222222 Medica 2
					333333 Medico 3
					444444 Medico 4

[EXERCICIO 1](Sistema_Medico_Paciente/SMP001.COB)

    PROGRAMA :  SMP001 CADASTRO DO CEP
    ARQUIVO  :  CADCEP
    REGISTRO :  REGCEP

    CAMPOS DO REGISTRO :
    CEP              9(08)
    ENDERECO         X(30)
    BAIRRO           X(20)
    CIDADE           X(35)
    UF               X(02)

    TABELA UF
    SP- "SAO PAULO"
    RJ- "RIO DE JANEIRO"
    PR- "PARANA"
    RS- "RIO GRANDE DO SUL"
    SC- "SANTA CATARINA"
    MG- "MINAS GERAIS"
    MT- "MATO GROSSO"

    OBS.: TODOS OS CAMPOS DEVEM SER CONSISTIDOS
          CAMPOS NUMÉRICOS NÃO PODEM SER ZERO.
          CAMPOS ALFANUMÉRICOS NÃO PODE FICAR EM BRANCO (EXCETO COMPLEMENTO).


[EXERCÍCIO 2](Sistema_Medico_Paciente/SMP002.COB)

    PROGRAMA : SMP002 CADASTRO DA CID (CID = DOENÇA)
    ARQUIVO  : CADCID
    REGISTRO : REGCID

    CAMPOS DO REGISTRO :

    CODCID            9(04)
    DENOMINACAO       X(30)

    OBS.: TODOS OS CAMPOS DEVEM SER CONSISTIDOS
          CAMPOS NUMÉRICOS NÃO PODEM SER ZERO.
          CAMPOS ALFANUMÉRICOS NÃO PODE FICAR EM BRANCO (EXCETO COMPLEMENTO).
      
  

[EXERCÍCIO 3](Sistema_Medico_Paciente/SMP003.COB)

    PROGRAMA : SMP003 CADASTRO DO CONVENIO 
    ARQUIVO  :  CADCONV
    REGISTRO :  REGCONV

    CAMPOS DO REGISTRO :
    CODIGO         9(04)
    NOME CONVENIO  X(30)
    PLANO          9(02)

    TABELA PLANOS
    01 –ENFERMARIA REGIONAL
    02 –ENFERMARIA NACIONAL
    03 -ENFERMARIA INTERNACIONAL
    04 –APTO PADRÃO REGIONAL
    05 –APTO PADRAO NACIONAL
    06 -APTO PADRAO INTERNACIONAL
    07 –EMERGENCIA REGIONAL
    08 –EMERGENCIA NACIONAL
    09 –EMERCENCIA INTERNACIONAL
    10 –PLANO GLOBAL

    OBS.: TODOS OS CAMPOS DEVEM SER CONSISTIDOS
          CAMPOS NUMÉRICOS NÃO PODEM SER ZERO.
          CAMPOS ALFANUMÉRICOS NÃO PODE FICAR EM BRANCO (EXCETO COMPLEMENTO).
          DATA NASCIMENTO TEM QUE SER UMA DATA VALIDA.
          PLANO NÃO PODE SER ZERO.
      

[EXERCICIO 4](Sistema_Medico_Paciente/SMP004.COB)

    PROGRAMA : SMP004 CADASTRO DE PACIENTES
    ARQUIVO  :  CADPACI
    REGISTRO :  REGPACI

    CAMPOS DO REGISTRO :

    CPF            9(11)
    NOME PACIENTE  X(30)
    DATANASC
      DIA          9(02)
      MES          9(02)
      ANO          9(04)
    SEXO           X(01)
    GENERO         X(01)
    CONVENIO       9(04)
    LOGRADOURO
      ACEP         9(08)
      NUMERO       9(04)
      COMPLEMENTO  X(12)
    TELEFONE
      DDD          9(03)
      NUM          9(09)
    EMAIL          X(30)
    ESPECIALIDADE  9(02)

    SEXO           X(01)

    TABELA DE GENERO
    E-HETEROSEXUAL
    B-BISEXUAL
    T-TRANSSEXUAL
    P-PANSEXUAL
    H-HOMOSEXUAL
    N-NAO DECLARADO
    O-OUTROS

    TABELA SEXO 
    M –MASCULINO
    F –FEMENINO

    TABELA PLANOS
    01 –ENFERMARIA REGIONAL
    02 –ENFERMARIA NACIONAL
    03 -ENFERMARIA INTERNACIONAL
    04 –APTO PADRÃO REGIONAL
    05 –APTO PADRAO NACIONAL
    06 -APTO PADRAO INTERNACIONAL
    07 –EMERGENCIA REGIONAL
    08 –EMERGENCIA NACIONAL
    09 –EMERCENCIA INTERNACIONAL
    10 –PLANO GLOBAL

    OBS.: TODOS OS CAMPOS DEVEM SER CONSISTIDOS
          CAMPOS NUMÉRICOS NÃO PODEM SER ZERO.
          CAMPOS ALFANUMÉRICOS NÃO PODE FICAR EM BRANCO (EXCETO COMPLEMENTO).
          DATA NASCIMENTO TEM QUE SER UMA DATA VALIDA.
          CONECTAR COM ARQUIVO DO SMP001 PARA CEP E SMP003 PARA CONVENIO.

[EXERCÍCIO 5](Sistema_Medico_Paciente/SMP005.COB)

    PROGRAMA : SMP005 CADASTRO DO MEDICO 
    ARQUIVO  : CADMED
    REGISTRO : REGMED

    CAMPOS DO REGISTRO :

    CRM            9(06)
    NOMEM          X(30)
    LOGRADOURO
      ACEP         9(08)
      NUMERO       9(04)
      COMPLEMENTO  X(12)
    TELEFONES
      CELULAR
        DDDCEL     9(03)
        NUMCEL     9(09)
      RESIDENCIAL
        DDDRES     9(03)
        NUMRES     9(08)
    EMAIL          X(35)
    ESPECIALIDADE  9(02)
    DATANASC
      DIA          9(02)
      MES          9(02)
      ANO          9(04)
    SEXO           X(01)


    TABELA ESPECIALIDADE
    01 -CLINICA MEDICA
    02 –UROLOGIA
    03 –GINICOLOGISTA
    04 –PEDIATRIA
    05 –CARDIOLOGISTA...   ( COLOCAR DIVERSAS ESPECIALIDADES )

    TABELA SEXO 
    M –MASCULINO
    F –FEMENINO

    OBS.: TODOS OS CAMPOS DEVEM SER CONSISTIDOS
          CAMPOS NUMÉRICOS NÃO PODEM SER ZERO.
          CAMPOS ALFANUMÉRICOS NÃO PODE FICAR EM BRANCO (EXCETO COMPLEMENTO).
          DATA NASCIMENTO TEM QUE SER UMA DATA VALIDA.
          CONECTAR COM ARQUIVO DO SMP001 PARA CEP.


[EXERCICIO 6](Sistema_Medico_Paciente/SMP006.COB)

    PROGRAMA : SMP006 CADASTRO DO CONSULTA
    ARQUIVO  :  CADCONSU
    REGISTRO :  REGCONSU

    CAMPOS DO REGISTRO :
    CPF PACIENTE   9(11)
    DATA CONSULTA
      DIA                9(02)
      MES                9(02)
      ANO                9(04)
    CRM MÉDICO           9(06)
    CONVENIO             9(04)
    CID                  9(04)
    DESCRIÇÃO CONSULTA1  X(60)
    DESCRIÇÃO CONSULTA2  X(60)

    OBS.: TODOS OS CAMPOS DEVEM SER CONSISTIDOS
          CAMPOS NUMÉRICOS NÃO PODEM SER ZERO.
          CAMPOS ALFANUMÉRICOS NÃO PODE FICAR EM BRANCO (EXCETO COMPLEMENTO).
          DATA CONSULTA TEM QUE SER UMA DATA VALIDA.
          CONECTAR COM ARQUIVO DO SMP002 PARA CID, SMP003 PARA CONVENIO, SMP004 PARA PACIENTE E SMP005 PARA MEDICO.
          EXIBIR AS INFORMAÇÕES CONFORME TELA ABAIXO:
![Imagem contendo a tela do sistema em COBOL solicitada pelo professor](https://github.com/juliobarcellos/Programacao_Mainframe/blob/main/tela%20prova.png)
