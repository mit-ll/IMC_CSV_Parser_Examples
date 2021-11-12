//DISTRIBUTION STATEMENT A. Approved for public release. Distribution is unlimited.

//This material is based upon work supported by the Under Secretary of Defense for Research and Engineering under Air Force Contract No. FA8702-15-D-0001. Any opinions, findings, conclusions or recommendations expressed in this material are those of the author(s) and do not necessarily reflect the views of the Under Secretary of Defense for Research and Engineering.

//Subject to FAR52.227-11 Patent Rights - Ownership by the contractor (May 2014)

//Delivered to the U.S. Government with Unlimited Rights, as defined in DFARS Part 252.227-7013 or 7014 (Feb 2014). Notwithstanding any copyright notice, U.S. Government rights in this work are defined by DFARS 252.227-7013 or DFARS 252.227-7014 as detailed above. Use of this work other than as specifically authorized by the U.S. Government may violate any copyrights that exist in this work.

//© 2021 Massachusetts Institute of Technology.

//Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

// 1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.

// 2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

module csvparser
 
  /// Events during a CSV file parse run
  type actions = 
      | EndField of int // End of field at offset zero usage, but 1 in a few cases where there is look-ahead.
      | EndRecord // End of the line of fields.
      | BeginQuote // Begin the state of matching a quoted field
      | QuoteQuote // Handle a quoted quote  ""
      | BeginField // Begin an unquoted field
      | Error  // in a state that should not be possible.

  /// Trivial class to emulate a byte of TCAM
  type TCAM_Byte(t,m) = 
    class
      let mutable target : byte = t &&& m
      let mutable mask : byte = m

      // **** **** constructor
      new() = TCAM_Byte(0uy,0uy)

      // Byte literal constructor
      new (bytevalue) = TCAM_Byte(bytevalue,0xffuy)

      // Shorthand to grab ascii from a char for a byte literal  Do not use with chars of value higher than 127
      new(literal : char) =
        let bytevalue = literal |> byte
        TCAM_Byte(bytevalue)

      // tests a byte for a match against the tcam
      member __.Contains(test : byte) = target = (test &&& mask)
    end

  /// Basically, the sequence of events to call, the amount to move the state machine cursor, the next state, and a tag for debugging state machines 
  type stateAction = {events : list<actions>; skip : int; next : byte; tag : int}

  module Classifiers = begin

    // Var-2 State Machine for parsing CSV files
    let NOcsv = [|
      //state field beginning
      (TCAM_Byte(0uy),TCAM_Byte(','),TCAM_Byte()), {events=[BeginField;EndField(0)];skip=1;next=0uy;tag=0};
      (TCAM_Byte(0uy),TCAM_Byte('"'),TCAM_Byte()), {events=[BeginQuote];skip=1;next=2uy;tag=1};
      (TCAM_Byte(0uy),TCAM_Byte('\n'),TCAM_Byte('\r')), {events=[BeginField;EndField(0);EndRecord];skip=2;next=0uy;tag=3};
      (TCAM_Byte(0uy),TCAM_Byte('\r'),TCAM_Byte('\n')), {events=[BeginField;EndField(0);EndRecord];skip=2;next=0uy;tag=4};
      (TCAM_Byte(0uy),TCAM_Byte('\n'),TCAM_Byte()), {events=[BeginField;EndField(0);EndRecord];skip=1;next=0uy;tag=5};
      (TCAM_Byte(0uy),TCAM_Byte('\r'),TCAM_Byte()), {events=[BeginField;EndField(0);EndRecord];skip=1;next=0uy;tag=6};
      (TCAM_Byte(0uy),TCAM_Byte(0x1euy),TCAM_Byte()), {events=[BeginField;EndField(0);EndRecord];skip=1;next=0uy;tag=7};

      (TCAM_Byte(0uy),TCAM_Byte(),TCAM_Byte()), {events=[BeginField];skip=1;next=1uy;tag=8};
      // state unquoted field
      (TCAM_Byte(1uy),TCAM_Byte(','),TCAM_Byte()), {events=[EndField(0)];skip=1;next=0uy;tag=09};
      (TCAM_Byte(1uy),TCAM_Byte('\n'),TCAM_Byte('\r')), {events=[EndField(0);EndRecord];skip=2;next=0uy;tag=10};
      (TCAM_Byte(1uy),TCAM_Byte('\r'),TCAM_Byte('\n')), {events=[EndField(0);EndRecord];skip=2;next=0uy;tag=11};
      (TCAM_Byte(1uy),TCAM_Byte('\n'),TCAM_Byte()), {events=[EndField(0);EndRecord];skip=1;next=0uy;tag=12};
      (TCAM_Byte(1uy),TCAM_Byte('\r'),TCAM_Byte()), {events=[EndField(0);EndRecord];skip=1;next=0uy;tag=13};
      (TCAM_Byte(1uy),TCAM_Byte(0x1euy),TCAM_Byte()), {events=[EndField(0);EndRecord];skip=1;next=0uy;tag=14};

      (TCAM_Byte(1uy),TCAM_Byte(),TCAM_Byte(',')), {events=[EndField(1)];skip=2;next=0uy;tag=15};

      (TCAM_Byte(1uy),TCAM_Byte(),TCAM_Byte(0x1euy)), {events=[EndField(1);EndRecord];skip=2;next=0uy;tag=16};
      (TCAM_Byte(1uy),TCAM_Byte(),TCAM_Byte('\n')), {events=[];skip=1;next=1uy;tag=17};
      (TCAM_Byte(1uy),TCAM_Byte(),TCAM_Byte('\r')), {events=[];skip=1;next=1uy;tag=18};
      
      (TCAM_Byte(1uy),TCAM_Byte(),TCAM_Byte()), {events=[];skip=2;next=1uy;tag=19};
      // state quoted field
      (TCAM_Byte(2uy),TCAM_Byte('"'),TCAM_Byte(',')), {events=[EndField(0)];skip=2;next=0uy;tag=20};
      
      (TCAM_Byte(2uy),TCAM_Byte('"'),TCAM_Byte('"')), {events=[QuoteQuote];skip=2;next=2uy;tag=21};
      (TCAM_Byte(2uy),TCAM_Byte('"'),TCAM_Byte(0x1euy)), {events=[EndField(0);EndRecord];skip=2;next=0uy;tag=22};
      (TCAM_Byte(2uy),TCAM_Byte('"'),TCAM_Byte('\r')), {events=[EndField(0)];skip=1;next=3uy;tag=23};
      (TCAM_Byte(2uy),TCAM_Byte('"'),TCAM_Byte('\n')), {events=[EndField(0)];skip=1;next=3uy;tag=24};
      
      (TCAM_Byte(2uy),TCAM_Byte(),TCAM_Byte('"')), {events=[];skip=1;next=2uy;tag=25};
      (TCAM_Byte(2uy),TCAM_Byte(),TCAM_Byte()), {events=[];skip=2;next=2uy;tag=26};
      // finishing a state quoted field
      (TCAM_Byte(3uy),TCAM_Byte('\n'),TCAM_Byte('\r')), {events=[EndRecord];skip=2;next=0uy;tag=27};
      (TCAM_Byte(3uy),TCAM_Byte('\r'),TCAM_Byte('\n')), {events=[EndRecord];skip=2;next=0uy;tag=28};
      (TCAM_Byte(3uy),TCAM_Byte('\n'),TCAM_Byte()), {events=[EndRecord];skip=1;next=0uy;tag=029};
      (TCAM_Byte(3uy),TCAM_Byte('\r'),TCAM_Byte()), {events=[EndRecord];skip=1;next=0uy;tag=30};
    |]

    /// Helper routine to find the decision in the LUT
    let find_decision tcam_table default_decision (item : uint32) = 
      let x = tcam_table 
              |> Array.tryFind 
                (fun ((a : TCAM_Byte, b:TCAM_Byte, c:TCAM_Byte),_) -> 
                  a.Contains(item >>> 16 |> byte) && b.Contains(item >>> 8 |> byte) && c.Contains(item |> byte) 
                )     
      match x with Some (_,y) -> y | None -> default_decision

    /// Builds a SRAM LUT for a certain bit width for a lookup table
    let build_LUT bits tcam_table = 
      Array.init ((1u <<< bits) |> int) (fun i -> (find_decision tcam_table {events=[Error];skip=0;next=0uy;tag=789} (uint32 i)) )
  end

  let build_sample_csv_parser beginfield endfield endrecord beginquote quotequote error = 
    let LUT = Classifiers.build_LUT 18 Classifiers.NOcsv
    fun (arr : byte []) ->
      
      let mutable cursor = 0 // offset in arr to take the next byte
      let mutable lookUpBuffer = 0us // two bytes in the buffer for using in the LUT
      let inline offseteof() = cursor >= arr.Length  // helper function to detect offset is out of bounds
      let inline pos() = cursor - 2 // helper function to calculate the cursor as seen by the state machine
      let inline eof() = pos() >= arr.Length // helper function to calcuate if the Lookupbuffer is out of bounds
      let inline skipOne() = // advance the lookup buffer one byte in arr
        if offseteof() then
          lookUpBuffer <- lookUpBuffer <<< 8
          if not <| eof() then cursor <- cursor + 1          
        else
          lookUpBuffer <- (lookUpBuffer <<< 8) ||| (uint16 arr.[cursor])
          cursor <- cursor + 1

      let mutable state = 0uy  // Current state
      do skipOne(); skipOne()  // fill lookupBuffer

      let mutable LUTCounts = 0u // Count the number lookups needs to parse arr

      while not <| eof() do // keep going until no more input
        // build lookup key
        let lk = ((uint32 state) <<< 16) ||| (uint32 lookUpBuffer)
        let act = LUT.[int lk] // do lookup
        LUTCounts <- LUTCounts + 1u
        //eprintfn "%A @ %i" act (pos())
        act.events // Do actions
        |> List.iter (
                      function
                      | BeginField -> do beginfield (pos())
                      | EndField x -> do endfield (pos() + x)
                      | EndRecord -> do endrecord ()
                      | BeginQuote -> do beginquote(pos() + 1)
                      | QuoteQuote -> do quotequote (pos())
                      | Error -> do if error() then (failwith "Parser Crashed") else () 
        )
        for _ = 1 to act.skip do skipOne () //move buffer up
        do state <- act.next // set next state

      LUTCounts

  module Samples = begin

    /// Simple parser that quotes all the fields in a csv
    let Sample_CSV_Parser (nullwrite : bool) (file : System.IO.FileInfo) = 

      let quote (x : string) = x.Replace("\"","\"\"")

      let mutable arr : byte[] = Array.empty
      
      // state for a record and fields with and ascii encoder
      let record = System.Collections.Generic.List<string>()
      let field = System.Text.StringBuilder()
      let encoder = System.Text.ASCIIEncoding.ASCII
      // detect if a field needs to be closed at the end of the file
      let mutable fieldopen = false
      // starting offset of the field
      let mutable fieldStart = 0
      // Callbacks for parsing events
      let inline beginfield x = fieldStart <- x; fieldopen <- true ; field.Clear() |> ignore
      let inline endfield x = 
        let cnt = x - fieldStart
        field.Append(encoder.GetString(arr,fieldStart,cnt)) |> ignore
        record.Add(field.ToString())
        fieldopen <- false
      let inline endrecord () =
        // Not the most efficient but it works
        if not nullwrite then record |> Seq.map (fun i -> quote i |> sprintf "\"%s\"") |> String.concat "," |> printfn "%s"
        record.Clear()
      // beginquote is the same as beginfield
      let inline quotequote x = 
        let cnt = x - fieldStart
        field.Append(encoder.GetString(arr,fieldStart,cnt)) |> ignore
        fieldStart <- x + 1
      let error () = true

      // build parser  (attached event processing to complete parser)
      let parser = build_sample_csv_parser beginfield endfield endrecord beginfield quotequote error
        
      // load file into memory
      do arr <- System.IO.File.ReadAllBytes(file.FullName)

      let ts = System.Diagnostics.Stopwatch()
      do  ts.Start()
      let cnt = parser arr
      do if fieldopen then endfield (arr.Length)
      do if record.Count > 0 then endrecord ()
      do ts.Stop()
      
      // some stats
      eprintfn "Lookups to LUT : %i; number of bytes : %i; average stride : %2.6f" cnt arr.Length ((float arr.Length) / (float cnt))
      eprintfn "time %ims;  Throughput %2.4f Bytes/ms" ts.ElapsedMilliseconds ((float arr.Length) / (float ts.ElapsedMilliseconds))



  end