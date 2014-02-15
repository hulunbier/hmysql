# hmysql

A (low level) MySQL driver in pure Haskell, supports handling large resultset in streaming mode.

Currently, it is just a very basic and naive implementation.



# Build & Play


`cabal build`

(If in trouble, I suggest using *cabal sanbox*)

For demostration, a very simple MySQL client repl is provided.

`./dist/build/repl/repl -u$username --pass$password -d$database`

or `./dist/build/repl/repl -?` for more argvs.

then, feed it with some sqls,

    Flags {host = "localhost", port = "3306", user = "root", pass = "", db = ""}
    Greeting
      { protocol = 10
      , version = "5.5.35-0ubuntu0.13.10.2"
      , tid = 57
      , salt1 = "yi$s8]MI"
      , caps = 63487
      , lang = 8
      , status = 2
      , salt2 = "eBZ{!-[ekiO-"
      }
    hmysql> select 1
    [ Packed
        { header = PacketHeader { hLen = 2 , hSeq = 4 }
        , payload = RowData [ LCS "1" ]
        }
    ]
    
for verbose output, just prefix ':' to the sql,
  
    hmysql> :select 1
    RSRes
      ResultSetPackets
        { rColCount =
            Packed
              { header = PacketHeader { hLen = 1 , hSeq = 1 }
              , payload = ColCount 1
              }
        , rColDefs =
            [ Packed
                { header = PacketHeader { hLen = 23 , hSeq = 2 }
                , payload =
                    ColDef
                      { fdb = LCS ""
                      , ftbl = LCS ""
                      , forgTbl = LCS ""
                      , fName = LCS "1"
                      , forgName = LCS ""
                      , fcharset = 63
                      , flen = 1
                      , ftype = 8
                      , flags = 129
                      , fdecimals = 0
                      }
                }
            ]
        , rEOF1 =
            Packed
              { header = PacketHeader { hLen = 5 , hSeq = 3 }
              , payload = EOF { warningCount = 0 , statusFlags = 2 }
              }
        , rRows =
            [ Packed
                { header = PacketHeader { hLen = 2 , hSeq = 4 }
                , payload = RowData [ LCS "1" ]
                }
            ]
        , rEOF2 =
            Packed
              { header = PacketHeader { hLen = 5 , hSeq = 5 }
              , payload = EOF { warningCount = 0 , statusFlags = 2 }
              }
        }

for streaming mode, prefix "!" to the sql

    hmysql> use somedb
    OKRes
      Packed
        { header = PacketHeader { hLen = 7 , hSeq = 1 } , payload = OK }
    hmysql> select count(*) from a_large_table
    [ Packed
        { header = PacketHeader { hLen = 7 , hSeq = 4 }
        , payload = RowData [ LCS "214573185" ]
        }
    ]
    hmysql> !select * from a_large_table --warning: lots of outputs! but can be processed in constant space.
    [ Packed
        { header = PacketHeader { hLen = 250 , hSeq = 25 }
        , payload =
            RowData
              [ LCS "084"
              , LCS "2013-08-08 16:09:23"
              , LCS "2"
              ...
              , Null
              , Null
              ]
        }
    ]
    
    ...
        

# Bugs & Known Issues

* Does not support MySQL packet larger than 16Mbytes. 
* Does not support multi-resultset yet.
* Lack of MySQL packet validations, such as sequence num, integrity of packets, etc.
* Stream resultset accessing API is ... awkward
* ...
* 
