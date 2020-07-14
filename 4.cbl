       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGRAM-4.
       AUTHOR. NICHOLAS-MORRISON.
      ****************************************************************
      * This program was supposed to sort and merge three seperate
      * input files from the different warehouses. It would then print 
      * a document showing the name, id, size, type, stock, and cost
      * of the product. It would use control breaks in order to 
      * calculate the totals for the warehouse, vendor, and product.
      *
      *
      *
      *
      *
      *
      *
      *
      *
      *
      *
      *
      *
      *
      *
      *
      *
      *
      *
      *
      *
      *
      *
      *
      *
      *
      ****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. MAC-COMPUTER.
       OBJECT-COMPUTER. WINDOWS-PC.
      
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
          SELECT WAREHOUSE1-FILE
             ASSIGN TO 'PR4F19-CA20.TXT'
             ORGANIZATION IS LINE SEQUENTIAL.
          SELECT WAREHOUSE2-FILE
             ASSIGN TO 'PR4F19-NV10.TXT'
             ORGANIZATION IS LINE SEQUENTIAL.
          SELECT WAREHOUSE3-FILE
             ASSIGN TO 'PR419-WA30.TXT'
             ORGANIZATION IS LINE SEQUENTIAL.
          SELECT WH1-SORTED-FILE
             ASSIGN TO 'SORTED-PR4F19-CA20.TXT'
             ORGANIZATION IS LINE SEQUENTIAL.
          SELECT WH2-SORTED-FILE
             ASSIGN TO 'SORTED-PR4F19-WA30.TXT'
             ORGANIZATION IS LINE SEQUENTIAL.
          SELECT WH3-SORTED-FILE
             ASSIGN TO 'SORTED-PR4F19-WA30.TXT'
             ORGANIZATION IS LINE SEQUENTIAL.
          SELECT MERGED-WH-FILE
             ASSIGN TO 'MERGED-SORTED-WH.TXT'
             ORGANIZATION IS LINE SEQUENTIAL.
          SELECT SORT-FILE
             ASSIGN TO 'SORT.TMP'.
          SELECT WH-REPORT
             ASSIGN TO PRINTER 'WH-REPORT.txt'.
       
       DATA DIVISION.
       FILE SECTION.
       
       
       
       FD  WAREHOUSE1-FILE
           RECORD CONTAINS 128 CHARACTERS.
      
        01  UWH1-RECORD.
            05 UWH1-WAREHOUSE-ID       PIC X(4).
            05 UWH1-VENDOR-ID          PIC X.
            05 UWH1-PRODUCT-ID         PIC X(3).
            05 UWH1-DATA-ARRAY OCCURS 5 TIMES.
               10 UWH1-PRODUCT-NAME    PIC X(13).
               10 UWH1-PRODUCT-SIZE    PIC A.
               10 UWH1-PRODUCT-TYPE    PIC A.
               10 UWH1-NUM-STOCK       PIC S9(4).
               10 UWH1-PURCHACE-PRICE  PIC S999V99.
      
      
       FD  WAREHOUSE2-FILE
           RECORD CONTAINS 128 CHARACTERS.
           
       01  UWH1-RECORD.
            05 UWH2-WAREHOUSE-ID       PIC X(4).
            05 UWH2-VENDOR-ID          PIC X.
            05 UWH2-PRODUCT-ID         PIC X(3).
            05 UWH2-DATA-ARRAY OCCURS 5 TIMES.
               10 UWH2-PRODUCT-NAME    PIC X(13).
               10 UWH2-PRODUCT-SIZE    PIC A.
               10 UWH2-PRODUCT-TYPE    PIC A.
               10 UWH2-NUM-STOCK       PIC S9(4).
               10 UWH2-PURCHACE-PRICE  PIC S999V99.
          
       FD WAREHOUSE3-FILE
          RECORD CONTAINS 128 CHARACTERS.
       
       01  UWH2-RECORD.
            05 UWH3-WAREHOUSE-ID       PIC X(4).
            05 UWH3-VENDOR-ID          PIC X.
            05 UWH3-PRODUCT-ID         PIC X(3).
            05 UWH3-DATA-ARRAY OCCURS 5 TIMES.
               10 UWH3-PRODUCT-NAME    PIC X(13).
               10 UWH3-PRODUCT-SIZE    PIC A.
               10 UWH3-PRODUCT-TYPE    PIC A.
               10 UWH3-NUM-STOCK       PIC S9(4).
               10 UWH3-PURCHACE-PRICE  PIC S999V99.   
         
       
       
       FD  WH1-SORTED-FILE
           RECORD CONTAINS 128 CHARACTERS.
           
       01  WH1-RECORD.
           05  WR1-WAREHOUSE-ID             PIC X(4).
           05  WR1-VENDER-ID                PIC X.
           05  WR1-PRODUCT-ID               PIC X(3).
           05  WR1-PRODUCT-DATA OCCURS 5 TIMES.
               10  WR1-PD-NAME              PIC X(13).
               10  WR1-PD-SIZE              PIC A.
               10  WR1-PD-TYPE              PIC A.
               10  WR1-PD-NUM-IN-STOCK      PIC S9(4).
               10  WR1-PD-PRICE             PIC S999V99.
               
               
               
       FD  WH2-SORTED-FILE
           RECORD CONTAINS 128 CHARACTERS.
           
       01  WH2-RECORD.
           05  WR2-WAREHOUSE-ID             PIC X(4).
           05  WR2-VENDER-ID                PIC X.
           05  WR2-PRODUCT-ID               PIC X(3).
           05  WR2-PRODUCT-DATA OCCURS 5 TIMES.
               10  WR2-PD-NAME              PIC X(13).
               10  WR2-PD-SIZE              PIC A.
               10  WR2-PD-TYPE              PIC A.
               10  WR2-PD-NUM-IN-STOCK      PIC S9(4).
               10  WR2-PD-PRICE             PIC S999V99.
               
       FD  WH3-SORTED-FILE
           RECORD CONTAINS 128 CHARACTERS.
           
       01  WH3-RECORD.
           05  WR3-WAREHOUSE-ID             PIC X(4).
           05  WR3-VENDER-ID                PIC X.
           05  WR3-PRODUCT-ID               PIC X(3).
           05  WR3-PRODUCT-DATA OCCURS 5 TIMES.
               10  WR3-PD-NAME              PIC X(13).
               10  WR3-PD-SIZE              PIC A.
               10  WR3-PD-TYPE              PIC A.
               10  WR3-PD-NUM-IN-STOCK      PIC S9(4).
               10  WR3-PD-PRICE             PIC S999V99.        
       
       
       FD  MERGED-WH-FILE
           RECORD CONTAINS 128 CHARACTERS.
           
       01  MERGED-WH-RECORD.
           05  MWR-WAREHOUSE-ID             PIC X(4).
           05  MWR-VENDOR-ID                PIC X.
           05  MWR-PRODUCT-ID               PIC X(3).
           05  MWR-PRODUCT-DATA OCCURS 5 TIMES.
               10  MWR-PD-NAME              PIC X(13).
               10  MWR-PD-SIZE              PIC A.
               10  MWR-PD-TYPE              PIC A.
               10  MWR-PD-NUM-IN-STOCK      PIC S9(4).
               10  MWR-PD-PRICE             PIC S999V99.
               
       SD  SORT-FILE
           RECORD CONTAINS 128 CHARACTERS. 
           
       01  SORT-RECORD.
           05  SR-WAREHOUSE-ID             PIC X(4).
           05  SR-VENDOR-ID                PIC X.
           05  SR-PRODUCT-ID               PIC XX.
           05  SR-PRODUCT-DATA OCCURS 5 TIMES.
               10  SR-PD-NAME              PIC X(13).
               10  SR-PD-SIZE              PIC A.
               10  SR-PD-TYPE              PIC A.
               10  SR-PD-NUM-IN-STOCK      PIC S9(4).
               10  SR-PD-PRICE             PIC S999V99.
           
           
               
       FD  WH-REPORT
           RECORD CONTAINS 80 CHARACTERS.
           
       01  REPORT-RECORD                   PIC X(80).
       
       WORKING-STORAGE SECTION. 
       
       01 FLAGS-N-SWITCHES. 
          05  EOF-FLAG                  PIC X       VALUE ' '.
          05  FIRST-RECORD              PIC X(3)    VALUE 'YES'.
              88  NO-MORE-DATA                      VALUE 'N'.
          05  SUB                       PIC 9       VALUE 1.
          05  ERRORS                    PIC S99     VALUE +0.
       
       
       01 ERROR-REPORT.
          05  ERROR-REC                 PIC X(80). 
          
       01 REPORT-FIELDS. 
          05  PROPER-SPACING            PIC S9      VALUE +3.
          05  CURRENT-PAGE-NUMBER       PIC S99     VALUE +0.
          
       01 TEMP-DATE.
          05  TEMP-YEAR                  PIC 9999.
          05  TEMP-MONTH                 PIC 99.
          05  TEMP-DAY                   PIC 99.
          
       01 TEMP-FIELDS.
           05 PRODUCT-HOLD              PIC X(3).
           05 WAREHOUSE-HOLD            PIC X(4).
           05 VENDOR-HOLD               PIC X(12).
           
       01 TOTAL-FIELDS.
          05 PRODUCT-TOTAL              PIC 9(6)V99.
          05 WAREHOUSE-TOTAL            PIC 9(8)V99.
          05 VENDOR-TOTAL               PIC 9(7)V99.
          05 GRAND-TOTAL                PIC 9(9)V99.
          05 NAME-TOTAL                 PIC 9(7)V99.
          
      ************************ OUTPUT AREA **************************
      
       01  HEADER-L1.
           05  FILLER                   PIC X(33)   VALUE SPACES.
           05                           PIC X(9)
                                            VALUE 'DR. CHEEB'.
           05  FILLER                   PIC X(21)   VALUE SPACES. 
           
       01  HEADER-L2.
           05  FILLER                   PIC X(8)    VALUE SPACES. 
           05  HL2-DAY                  PIC 9(2).
           05                           PIC X       VALUE '/'.
           05  HL2-MONTH                PIC 9(2).
           05                           PIC X       VALUE '/'.
           05  HL2-YEAR                 PIC 9(4).
           05  FILLER                   PIC X(12)   VALUE SPACES. 
           05                           PIC X(16)   
                                            VALUE 'INVENTORY REPORT'.
           05  FILLER                   PIC X(8)    VALUE SPACES.
           05                           PIC X(6)
                                            VALUE 'PAGE: '.
           05  HEADER-PAGE-NUM                 PIC 99.
           05  FILLER                   PIC X       VALUE SPACES. 
           
       01  WAREHOUSE-HEADER.
           05  FILLER                   PIC X(2)    VALUE SPACES. 
           05                           PIC X(11)   
                                            VALUE 'WAREHOUSE: '.
           05  WH-WAREHOUSE             PIC X(4).
           05  FILLER                   PIC X(48)    VALUE SPACES.
           
       01   VENDOR-HEADER.
           05  FILLER                   PIC X(5)     VALUE SPACES.
           05                           PIC X(8)
                                            VALUE 'VENDOR: '.
           05  VH-VENDOR                PIC X(12).
           05  FILLER                   PIC X(40).
           
       01  INVALID-VENDOR.
          05                            PIC X(5)     VALUE SPACES.
          05                            PIC X(9)     
                                            VALUE 'VENDOR: '.
          05  INVALID-VEND              PIC A(1).
          05                            PIC X(11)
                                            VALUE ' IS INVALID'.  
           
       01  HEADER-COLUMNS-L1.
           05  FILLER                   PIC X(8)     VALUE SPACES. 
           05                           PIC X(7)    
                                            VALUE 'PRODUCT'.
           05  FILLER                   PIC X(7)      VALUE SPACES.
           05                           PIC X(4)     
                                            VALUE 'PROD'.
           05  FILLER                   PIC X(4)      VALUE SPACES.
           05                           PIC XX 
                                            VALUE 'IN'.
           05  FILLER                   PIC X(7)      VALUE SPACES. 
           05                           PIC X(5)
                                            VALUE 'TOTAL'.
       
       01  HEADER-COLUMNS-L2.
           05  FILLER                   PIC X(10)      VALUE SPACES.    
           05                           PIC X(4)
                                            VALUE 'NAME'.
           05  FILLER                   PIC X(9)       VALUE SPACES.
           05                           PIC X(2)     
                                            VALUE 'ID'.
           05  FILLER                   PIC X(6)       VALUE SPACES.
           05                           PIC X(4)      
                                            VALUE 'SIZE'.
           05  FILLER                   PIC X(6)       VALUE SPACES.
           05                           PIC X(4) 
                                            VALUE 'TYPE'.
           05  FILLER                   PIC X(3)       VALUE SPACES.
           05                           PIC X(5)      
                                            VALUE 'STOCK'.
           05  FILLER                   PIC X(5)       VALUE SPACES. 
           05                           PIC X(4)      
                                            VALUE 'COST'.
           05  FILLLER                  PIC X(3)        VALUE SPACES.
           
       01  DETAIL-LINE.
           05  FILLER                   PIC X(5)        VALUE SPACES.
           05  DL-PROD-NAME             PIC X(13).
           05  FILLER                   PIC X(4)        VALUE SPACES.
           05  DL-PROD-ID               PIC X(3).
           05  FILLER                   PIC X(2)        VALUE SPACES.
           05  DL-PROD-SIZE             PIC X(11).
           05  FILLER                   PIC X(2)        VALUE SPACES.
           05  DL-PROD-TYPE             PIC X(5). 
           05  FILLER                   PIC X(3)        VALUE SPACES.
           05  DL-NUM-IN-STOCK          PIC Z9999.
           05  FILLER                   PIC X(3)        VALUE SPACES.
           05  DL-TOTAL-COST            PIC $$$,$$$.99.
           
       01  TOTAL-FOR-VENDOR-LINE.
           05  FILLER                   PIC X(13)       VALUE SPACES.
           05                           PIC X(18)
                                        VALUE 'TOTAL FOR VENDOR: '.  
           05  TL-VENDOR                PIC X(12).
           05  FILLER                   PIC X(10)       VALUE SPACES.
           05  TL-VENDOR-TOTAL          PIC $,$$$,$$$V99.
           
       01  TOTAL-FOR-WAREHOUSE-LINE.
           05  FILLER                   PIC X(10)       VALUE SPACES.
           05                           PIC X(21)
                                       VALUE 'TOTAL FOR WAREHOUSE: '. 
           05  TL-WAREHOUSE             PIC X(4).
           05  FILLER                   PIC X(17)       VALUE SPACES.
           05  TL-WAREHOUSE-TOTAL       PIC $$,$$$,$$$V99.
           

       01  TOTAL-FOR-PRODUCT.
           05                  PIC X(21)   VALUE SPACES.
           05                  PIC X(9)    
                                   VALUE 'PRODUCT: '.
           05 TL-PRODUCT-NAME  PIC X(13).
           05                  PIC X       VALUE SPACES.
           05                  PIC X(6)    
                                   VALUE 'TOTAL:'.
           05                  PIC X(4)    VALUE SPACES.
           05 TL-PRODUCT-TOTAL PIC 9(6)V99.


       01  GRAND-TOTAL-LINE. 
          05  FILLER                   PIC X(22)        VALUE SPACES.
          05                           PIC X(18) 
                                       VALUE 'GRAND TOTAL COST: '.
          05  FILLER                   PIC X(12)        VALUE SPACES.
          05  TL-GRAND-TOTAL           PIC 9(8)V99.
    
       PROCEDURE DIVISION.
       
       
       001-MAIN-MODULE.
          PERFORM 100-SORT-WH-FILES
          PERFORM 200-HSKPING-ROUTINE
          PERFORM 400-READ-FILE
          
          
          PERFORM 350-FINAL-ROUTINE
       .
          
          
          
          
       100-SORT-WH-FILES.
       
          SORT SORT-FILE
          ON ASCENDING KEY SR-WAREHOUSE-ID
                           SR-VENDOR-ID
                           SR-PRODUCT-ID               
         USING WAREHOUSE1-FILE
         GIVING WH1-SORTED-FILE
         
         
         
         SORT SORT-FILE
         ON ASCENDING KEY SR-WAREHOUSE-ID
                          SR-VENDOR-ID
                          SR-PRODUCT-ID                
         USING WAREHOUSE2-FILE
         GIVING WH2-SORTED-FILE
          
          
          
          SORT SORT-FILE
          ON ASCENDING KEY SR-WAREHOUSE-ID
                           SR-VENDOR-ID
                           SR-PRODUCT-ID
          USING WAREHOUSE3-FILE
          GIVING WH3-SORTED-FILE
          
          
          MERGE SORT-FILE
          ON ASCENDING KEY SR-WAREHOUSE-ID
                              SR-VENDOR-ID
                              SR-PRODUCT-ID
          USING WH1-SORTED-FILE, WH2-SORTED-FILE, WH3-SORTED-FILE
          GIVING MERGED-WH-FILE
          .
          
       200-HSKPING-ROUTINE.
          
             OPEN INPUT MERGED-WH-FILE
                  OUTPUT WH-REPORT
                  
             ACCEPT TEMP-DATE FROM DATE YYYYMMDD
            
             MOVE TEMP-DAY TO HL2-DAY
             MOVE TEMP-MONTH TO HL2-MONTH
             MOVE TEMP-YEAR TO HL2-YEAR
             
             PERFORM 250-HEADER-ROUTINE
             
             .
       250-HEADER-ROUTINE.
          MOVE 1 TO HEADER-PAGE-NUM
          
          MOVE HEADER-L1 TO REPORT-RECORD
             MOVE 2 TO PROPER-SPACING
             PERFORM 300-WRITE-A-LINE 
          MOVE HEADER-L2 TO REPORT-RECORD
             MOVE 1 TO PROPER-SPACING
             PERFORM 300-WRITE-A-LINE
          MOVE WAREHOUSE-HEADER TO REPORT-RECORD   
             MOVE 2 TO PROPER-SPACING 
             PERFORM 300-WRITE-A-LINE
          MOVE VENDOR-HEADER TO REPORT-RECORD
             MOVE 2 TO PROPER-SPACING
             PERFORM 300-WRITE-A-LINE
          MOVE HEADER-COLUMNS-L1 TO REPORT-RECORD
             MOVE 2 TO PROPER-SPACING 
             PERFORM 300-WRITE-A-LINE
          MOVE HEADER-COLUMNS-L2 TO REPORT-RECORD
             MOVE 1 TO PROPER-SPACING
             PERFORM 300-WRITE-A-LINE
       .
       
       
       275-PRINT-COLUMNS.
          MOVE HEADER-COLUMNS-L1 TO REPORT-RECORD
          MOVE 1 TO REPORT-RECORD
          PERFORM 300-WRITE-A-LINE
          
          MOVE HEADER-COLUMNS-L2 TO REPORT-RECORD
          PERFORM 300-WRITE-A-LINE
          .
       
       300-WRITE-A-LINE.
       
           WRITE REPORT-RECORD
              AFTER ADVANCING PROPER-SPACING
           .
       
       350-FINAL-ROUTINE.
           CLOSE MERGED-WH-FILE
                 WH-REPORT
           STOP RUN
           .
            
       400-READ-FILE.
          
          PERFORM UNTIL NO-MORE-DATA
             READ MERGED-WH-FILE
                AT END
                   MOVE 'N' TO EOF-FLAG
                NOT AT END
                
                   IF MWR-WAREHOUSE-ID = 'WA30' OR 'CA20' OR 'NV10'
                      PERFORM 500-PROCESS-WH-RECORD
                   ELSE
                      ADD 1 TO ERRORS
                   END-IF
                END-READ
             END-PERFORM
        .
        
        500-PROCESS-WH-RECORD.
           
           EVALUATE TRUE
           
              WHEN FIRST-RECORD = 'YES'
                 MOVE 'NO' TO FIRST-RECORD
                 
                 MOVE MWR-PRODUCT-ID TO PRODUCT-HOLD
                 MOVE MWR-WAREHOUSE-ID TO WAREHOUSE-HOLD
                 MOVE MWR-VENDOR-ID TO VENDOR-HOLD
                 
                 MOVE WAREHOUSE-HOLD TO WH-WAREHOUSE
                 MOVE WAREHOUSE-HEADER TO REPORT-RECORD
                 MOVE 3 TO PROPER-SPACING
                 PERFORM 300-WRITE-A-LINE
                 
                 
                 PERFORM 525-WRITE-VENDOR
                 PERFORM 550-WRITE-PRODUCT
                 
              WHEN MWR-WAREHOUSE-ID NOT EQUAL WAREHOUSE-HOLD
                 PERFORM 600-WAREHOUSE-BREAK
                 
                 MOVE WAREHOUSE-HOLD TO WH-WAREHOUSE
                 MOVE WAREHOUSE-HEADER TO REPORT-RECORD
                 MOVE 3 TO PROPER-SPACING
                 PERFORM 300-WRITE-A-LINE
                 
                 PERFORM 525-WRITE-VENDOR
                 PERFORM 550-WRITE-PRODUCT
                 
             WHEN MWR-VENDOR-ID NOT EQUAL VENDOR-HOLD
                 PERFORM 650-VENDOR-BREAK
                 
                 PERFORM 525-WRITE-VENDOR
                 PERFORM 550-WRITE-PRODUCT   
                 
             WHEN MWR-PRODUCT-ID NOT EQUAL PRODUCT-HOLD
                 PERFORM 700-PRODUCT-BREAK
                 
                 PERFORM 550-WRITE-PRODUCT
                 
                 
          END-EVALUATE
          
          PERFORM 725-SIZE-EVALUATE
          
          .
       
       525-WRITE-VENDOR.
          
          PERFORM 800-PROCESS-VENDOR
          
          IF MWR-VENDOR-ID = 'V' OR 'I' OR 'N' OR 'W'
             MOVE VENDOR-HOLD TO VH-VENDOR
             MOVE VENDOR-HEADER TO REPORT-RECORD
             MOVE 2 TO PROPER-SPACING 
             PERFORM 300-WRITE-A-LINE
             
          ELSE
             MOVE INVALID-VENDOR TO REPORT-RECORD
             MOVE 2 TO PROPER-SPACING
             PERFORM 300-WRITE-A-LINE
             
          END-IF
       .
       
       550-WRITE-PRODUCT.
          MOVE MWR-PRODUCT-ID TO PRODUCT-HOLD
          MOVE MWR-PRODUCT-ID TO DL-PROD-ID
          
          PERFORM 275-PRINT-COLUMNS

       .

       600-WAREHOUSE-BREAK.

         PERFORM 650-VENDOR-BREAK
         MOVE MWR-WAREHOUSE-ID TO WAREHOUSE-HOLD
         PERFORM 850-PRINT-WH-TOTAL
       .


       650-VENDOR-BREAK.
          
          PERFORM 700-PRODUCT-BREAK
          MOVE MWR-VENDOR-ID TO VENDOR-HOLD
          PERFORM 875-PRINT-VENDOR-TOTAL

       .

       700-PRODUCT-BREAK.

          MOVE PRODUCT-HOLD TO DL-PROD-ID
          PERFORM 900-PRINT-PRODUCT-TOTAL

       .

       725-SIZE-EVALUATE.

          PERFORM VARYING SUB FROM 1 BY 1 
             UNTIL SUB > 5

          IF MWR-PD-NUM-IN-STOCK(SUB) IS NUMERIC  
             MOVE MWR-PD-NUM-IN-STOCK(SUB) TO DL-NUM-IN-STOCK
          ELSE 
             MOVE ZEROS TO MWR-PD-NUM-IN-STOCK(SUB)
             MOVE MWR-PD-NUM-IN-STOCK(SUB) TO DL-NUM-IN-STOCK
          END-IF

          IF MWR-PD-PRICE(SUB) IS NOT NUMERIC
             MOVE ZEROS TO MWR-PD-PRICE(SUB)
          END-IF

          COMPUTE PRODUCT-TOTAL = MWR-PD-NUM-IN-STOCK(SUB) 
                                  * MWR-PD-PRICE(SUB)
          MOVE PRODUCT-TOTAL TO DL-TOTAL-COST

          EVALUATE TRUE
             WHEN MWR-PD-SIZE(SUB) = 'X'
                   MOVE 'Extra Large' TO DL-PROD-SIZE
               WHEN MWR-PD-SIZE(SUB) = 'L'
                   MOVE 'Large' TO DL-PROD-SIZE
               WHEN MWR-PD-SIZE(SUB) = 'M'
                   MOVE 'Medium' TO DL-PROD-SIZE
               WHEN MWR-PD-SIZE(SUB) = 'S'
                   MOVE 'Small' TO DL-PROD-SIZE
               WHEN MWR-PD-SIZE(SUB) = 'A'
                   MOVE 'Sample' TO DL-PROD-SIZE

           END-EVALUATE

           EVALUATE TRUE

               WHEN MWR-PD-TYPE(SUB) = 'C'
                   MOVE 'Cream' TO DL-PROD-TYPE

               WHEN MWR-PD-TYPE(SUB) = 'O'
                   MOVE 'Oil' TO DL-PROD-TYPE

           END-EVALUATE

           MOVE MWR-PD-NAME(SUB) TO DL-PROD-NAME
           MOVE DETAIL-LINE TO REPORT-RECORD
           MOVE 1 TO PROPER-SPACING
           PERFORM 300-WRITE-A-LINE

           END-PERFORM
       .
       
       800-PROCESS-VENDOR.
       
          EVALUATE TRUE
             WHEN MWR-VENDOR-ID = 'V'
                MOVE 'VISSON CORP.' TO VENDOR-HOLD
                
             WHEN MWR-VENDOR-ID = 'I'
                MOVE 'MADEINHOUSE' TO VENDOR-HOLD
            
             WHEN MWR-VENDOR-ID ='N'
                MOVE 'NETON LTD' TO VENDOR-HOLD
                
             WHEN MWR-VENDOR-ID = 'W'
                MOVE 'WEST CORP.' TO VENDOR-HOLD   
             
         END-EVALUATE
         .

       850-PRINT-WH-TOTAL.
          
          MOVE TOTAL-FOR-WAREHOUSE-LINE TO REPORT-RECORD
          MOVE 2 TO PROPER-SPACING
          PERFORM 300-WRITE-A-LINE
       .

       875-PRINT-VENDOR-TOTAL.
          
          PERFORM 800-PROCESS-VENDOR
          MOVE VENDOR-HOLD TO VH-VENDOR
          MOVE VENDOR-HOLD TO TL-VENDOR

          MOVE TOTAL-FOR-VENDOR-LINE TO REPORT-RECORD
          MOVE 2 TO PROPER-SPACING
          PERFORM 300-WRITE-A-LINE

       .

       900-PRINT-PRODUCT-TOTAL.
          
          MOVE PRODUCT-HOLD TO TL-PRODUCT-NAME

          IF DL-TOTAL-COST IS NUMERIC
             MOVE DL-TOTAL-COST TO NAME-TOTAL
             COMPUTE TL-PRODUCT-TOTAL = NAME-TOTAL + NAME-TOTAL

          MOVE TOTAL-FOR-PRODUCT TO REPORT-RECORD
          MOVE 2 TO PROPER-SPACING
          PERFORM 300-WRITE-A-LINE
       .












         
                
                  
                
                
                
          
        
      
       
       
       
       
             
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      