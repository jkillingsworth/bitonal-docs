module BitonalDocs.Compression

open System

//-------------------------------------------------------------------------------------------------

type CompressionType =
    | None
    | Group3OneDimensional
    | PackBits

type Pixel =
    | Black
    | White

type Bit =
    | Bit0
    | Bit1

//-------------------------------------------------------------------------------------------------

module Group3OneDimensional =

    type private CodeWordElement<'T> = { Black : 'T; White : 'T }

    let private createLookup values =
        let mapping = Seq.map (fun i -> if i = '0' then Bit0 else Bit1) >> Seq.toList
        let mapping element = { Black = mapping element.Black; White = mapping element.White }
        Map values |> Map.map (fun k -> mapping)

    let private codeWordsTerminating =
        createLookup
            [ 0000, { Black = "0000110111"   ; White = "00110101"     }
              0001, { Black = "010"          ; White = "000111"       }
              0002, { Black = "11"           ; White = "0111"         }
              0003, { Black = "10"           ; White = "1000"         }
              0004, { Black = "011"          ; White = "1011"         }
              0005, { Black = "0011"         ; White = "1100"         }
              0006, { Black = "0010"         ; White = "1110"         }
              0007, { Black = "00011"        ; White = "1111"         }
              0008, { Black = "000101"       ; White = "10011"        }
              0009, { Black = "000100"       ; White = "10100"        }
              0010, { Black = "0000100"      ; White = "00111"        }
              0011, { Black = "0000101"      ; White = "01000"        }
              0012, { Black = "0000111"      ; White = "001000"       }
              0013, { Black = "00000100"     ; White = "000011"       }
              0014, { Black = "00000111"     ; White = "110100"       }
              0015, { Black = "000011000"    ; White = "110101"       }
              0016, { Black = "0000010111"   ; White = "101010"       }
              0017, { Black = "0000011000"   ; White = "101011"       }
              0018, { Black = "0000001000"   ; White = "0100111"      }
              0019, { Black = "00001100111"  ; White = "0001100"      }
              0020, { Black = "00001101000"  ; White = "0001000"      }
              0021, { Black = "00001101100"  ; White = "0010111"      }
              0022, { Black = "00000110111"  ; White = "0000011"      }
              0023, { Black = "00000101000"  ; White = "0000100"      }
              0024, { Black = "00000010111"  ; White = "0101000"      }
              0025, { Black = "00000011000"  ; White = "0101011"      }
              0026, { Black = "000011001010" ; White = "0010011"      }
              0027, { Black = "000011001011" ; White = "0100100"      }
              0028, { Black = "000011001100" ; White = "0011000"      }
              0029, { Black = "000011001101" ; White = "00000010"     }
              0030, { Black = "000001101000" ; White = "00000011"     }
              0031, { Black = "000001101001" ; White = "00011010"     }
              0032, { Black = "000001101010" ; White = "00011011"     }
              0033, { Black = "000001101011" ; White = "00010010"     }
              0034, { Black = "000011010010" ; White = "00010011"     }
              0035, { Black = "000011010011" ; White = "00010100"     }
              0036, { Black = "000011010100" ; White = "00010101"     }
              0037, { Black = "000011010101" ; White = "00010110"     }
              0038, { Black = "000011010110" ; White = "00010111"     }
              0039, { Black = "000011010111" ; White = "00101000"     }
              0040, { Black = "000001101100" ; White = "00101001"     }
              0041, { Black = "000001101101" ; White = "00101010"     }
              0042, { Black = "000011011010" ; White = "00101011"     }
              0043, { Black = "000011011011" ; White = "00101100"     }
              0044, { Black = "000001010100" ; White = "00101101"     }
              0045, { Black = "000001010101" ; White = "00000100"     }
              0046, { Black = "000001010110" ; White = "00000101"     }
              0047, { Black = "000001010111" ; White = "00001010"     }
              0048, { Black = "000001100100" ; White = "00001011"     }
              0049, { Black = "000001100101" ; White = "01010010"     }
              0050, { Black = "000001010010" ; White = "01010011"     }
              0051, { Black = "000001010011" ; White = "01010100"     }
              0052, { Black = "000000100100" ; White = "01010101"     }
              0053, { Black = "000000110111" ; White = "00100100"     }
              0054, { Black = "000000111000" ; White = "00100101"     }
              0055, { Black = "000000100111" ; White = "01011000"     }
              0056, { Black = "000000101000" ; White = "01011001"     }
              0057, { Black = "000001011000" ; White = "01011010"     }
              0058, { Black = "000001011001" ; White = "01011011"     }
              0059, { Black = "000000101011" ; White = "01001010"     }
              0060, { Black = "000000101100" ; White = "01001011"     }
              0061, { Black = "000001011010" ; White = "00110010"     }
              0062, { Black = "000001100110" ; White = "00110011"     }
              0063, { Black = "000001100111" ; White = "00110100"     } ]

    let private codeWordsMakeup =
        createLookup
            [ 0064, { Black = "0000001111"   ; White = "11011"        }
              0128, { Black = "000011001000" ; White = "10010"        }
              0192, { Black = "000011001001" ; White = "010111"       }
              0256, { Black = "000001011011" ; White = "0110111"      }
              0320, { Black = "000000110011" ; White = "00110110"     }
              0384, { Black = "000000110100" ; White = "00110111"     }
              0448, { Black = "000000110101" ; White = "01100100"     }
              0512, { Black = "0000001101100"; White = "01100101"     }
              0576, { Black = "0000001101101"; White = "01101000"     }
              0640, { Black = "0000001001010"; White = "01100111"     }
              0704, { Black = "0000001001011"; White = "011001100"    }
              0768, { Black = "0000001001100"; White = "011001101"    }
              0832, { Black = "0000001001101"; White = "011010010"    }
              0896, { Black = "0000001110010"; White = "011010011"    }
              0960, { Black = "0000001110011"; White = "011010100"    }
              1024, { Black = "0000001110100"; White = "011010101"    }
              1088, { Black = "0000001110101"; White = "011010110"    }
              1152, { Black = "0000001110110"; White = "011010111"    }
              1216, { Black = "0000001110111"; White = "011011000"    }
              1280, { Black = "0000001010010"; White = "011011001"    }
              1344, { Black = "0000001010011"; White = "011011010"    }
              1408, { Black = "0000001010100"; White = "011011011"    }
              1472, { Black = "0000001010101"; White = "010011000"    }
              1536, { Black = "0000001011010"; White = "010011001"    }
              1600, { Black = "0000001011011"; White = "010011010"    }
              1664, { Black = "0000001100100"; White = "011000"       }
              1728, { Black = "0000001100101"; White = "010011011"    }
              1792, { Black = "00000001000"  ; White = "00000001000"  }
              1856, { Black = "00000001100"  ; White = "00000001100"  }
              1920, { Black = "00000001101"  ; White = "00000001101"  }
              1984, { Black = "000000010010" ; White = "000000010010" }
              2048, { Black = "000000010011" ; White = "000000010011" }
              2112, { Black = "000000010100" ; White = "000000010100" }
              2176, { Black = "000000010101" ; White = "000000010101" }
              2240, { Black = "000000010110" ; White = "000000010110" }
              2304, { Black = "000000010111" ; White = "000000010111" }
              2368, { Black = "000000011100" ; White = "000000011100" }
              2432, { Black = "000000011101" ; White = "000000011101" }
              2496, { Black = "000000011110" ; White = "000000011110" }
              2560, { Black = "000000011111" ; White = "000000011111" } ]

    let private calculateMakeupRunLength count =
        codeWordsMakeup
        |> Map.toList
        |> List.map fst
        |> List.filter (fun x -> x <= count)
        |> List.max

    let private getCodeWord element = function
        | Black -> element.Black
        | White -> element.White

    let compress scanline =

        let (|NextItemMatches|NextItemDiffers|End|) = function
            | (count, value), head :: tail when head = value
                -> NextItemMatches ((count, value), tail)
            | (count, value), head :: tail
                -> NextItemDiffers ((count, value), head, tail)
            | (count, value), []
                -> End (count, value)

        let rec loop acc state input =
            match state, input with
            | NextItemMatches ((count, value), tail) -> loop acc (count + 1, value) tail
            | NextItemDiffers (state, head, tail) -> loop (state :: acc) (1, head) tail
            | End (state) -> state :: acc

        let rec computeCodeWords acc = function
            | (count, value) when count < 64
                ->
                let runLength = count
                let codeWords = codeWordsTerminating
                let codeWord = getCodeWord codeWords.[runLength] value
                codeWord :: acc
            | (count, value)
                ->
                let runLength = calculateMakeupRunLength count
                let codeWords = codeWordsMakeup
                let codeWord = getCodeWord codeWords.[runLength] value
                let count = count - runLength
                computeCodeWords (codeWord :: acc) (count, value)
    
        let computeCodeWordsForRun =
            computeCodeWords [] >> List.reduce (fun x y -> y @ x)

        scanline
        |> List.ofArray
        |> loop [] (0, White)
        |> List.map computeCodeWordsForRun
        |> List.reduce (fun x y -> y @ x)
        |> List.toArray

//-------------------------------------------------------------------------------------------------

module PackBits =

    let private maxRunLength = 128

    let private (|BeginNextItemsToBePacked|BeginNextItemsToBeInline|End|) =
        function
        | head :: next1 :: next2 :: tail when head = next1 && head = next2
            -> BeginNextItemsToBePacked (head, next1 :: next2 :: tail)
        | head :: tail
            -> BeginNextItemsToBeInline (head, tail)
        | []
            -> End

    let private (|PackedHasReachedRunLimit|PackedNextItemToBePacked|PackedNextItemToBeInline|PackedHasReachedInputEnd|) =
        function
        | (count, items), input when count = (1 - maxRunLength)
            -> PackedHasReachedRunLimit (count, items, input)
        | (count, items), head :: tail when head = List.head items
            -> PackedNextItemToBePacked (count, items, tail)
        | (count, items), head :: tail
            -> PackedNextItemToBeInline (count, items, head :: tail)
        | (count, items), []
            -> PackedHasReachedInputEnd (count, items, List.empty<byte>)

    let private (|InlineHasReachedRunLimit|InlineNextItemToBePacked|InlineNextItemToBeInline|InlineHasReachedInputEnd|) =
        function
        | (count, items), input when count = (maxRunLength - 1)
            -> InlineHasReachedRunLimit (count, items, input)
        | (count, items), head :: next1 :: next2 :: tail when head = next1 && head = next2
            -> InlineNextItemToBePacked (count, items, head :: next1 :: next2 :: tail)
        | (count, items), head :: tail
            -> InlineNextItemToBeInline (count, items, head, tail)
        | (count, items), []
            -> InlineHasReachedInputEnd (count, items, List.empty<byte>)

    let compress scanline =

        let rec loop acc = function
            | BeginNextItemsToBePacked (head, tail) -> loopPacked acc ((0, [head]), tail)
            | BeginNextItemsToBeInline (head, tail) -> loopInline acc ((0, [head]), tail)
            | End -> acc
        and loopPacked acc = function
            | PackedHasReachedRunLimit (count, items, input)
            | PackedHasReachedInputEnd (count, items, input)
            | PackedNextItemToBeInline (count, items, input) -> loop ((count, items) :: acc) input
            | PackedNextItemToBePacked (count, items, tail)
                ->
                let state = count - 1, items
                loopPacked acc (state, tail)
        and loopInline acc = function
            | InlineHasReachedRunLimit (count, items, input)
            | InlineHasReachedInputEnd (count, items, input)
            | InlineNextItemToBePacked (count, items, input) -> loop ((count, items) :: acc) input
            | InlineNextItemToBeInline (count, items, head, tail)
                ->
                let state = count + 1, head :: items
                loopInline acc (state, tail)

        scanline
        |> List.ofArray
        |> loop []
        |> List.map (fun (count, items) -> (byte count) :: (List.rev items))
        |> List.reduce (fun x y -> y @ x)
        |> List.toArray

//-------------------------------------------------------------------------------------------------

let private convertPixelArrayToSeqPixels image =

    let rows = Array2D.length1 image
    let cols = Array2D.length2 image
    let computeValue row col = image.[row, col]
    let computeArray row = Array.init cols (computeValue row)
    Seq.init rows computeArray

let private convertBitsTo1BppBytes bits =

    let length = Array.length bits
    let stride = int (ceil (double length / 8.0))

    let rec reduceBits offset acc = function
        | append when append = 0 -> acc
        | append ->
            let index = offset + append - 1
            let pixel = match bits.[index] with Bit0 -> 0uy | Bit1 -> 1uy
            let value = acc ||| (pixel <<< (8 - append))
            reduceBits offset value (append - 1)

    let computeValue i =
        let offset = i * 8
        let append = Math.Min(8, length - offset)
        reduceBits offset 0uy append

    Array.init stride computeValue

let private convertPixelToBit = function
    | Black -> Bit1
    | White -> Bit0

let compress = function

    | None
        -> convertPixelArrayToSeqPixels
        >> Seq.map (Array.map convertPixelToBit)
        >> Seq.map convertBitsTo1BppBytes
        >> Seq.concat
        >> Seq.toArray

    | Group3OneDimensional
        -> convertPixelArrayToSeqPixels
        >> Seq.map Group3OneDimensional.compress
        >> Seq.map convertBitsTo1BppBytes
        >> Seq.concat
        >> Seq.toArray

    | PackBits
        -> convertPixelArrayToSeqPixels
        >> Seq.map (Array.map convertPixelToBit)
        >> Seq.map convertBitsTo1BppBytes
        >> Seq.map PackBits.compress
        >> Seq.concat
        >> Seq.toArray
