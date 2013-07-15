module BitonalDocs.Tiff

open System
open System.IO

//-------------------------------------------------------------------------------------------------

type ByteOrder =
    | LittleEndian = 0x4949us
    | BigEndian = 0x4d4dus

type Indicator =
    | Tiff = 42us

type Value =
    | Short of uint16
    | Long of uint32

type ValueOrOffset =
    | Value of Value
    | Offset of uint32

type ImageFileEntry =
    { Tag : uint16
      Type : uint16
      Count : uint32
      ValueOrOffset : ValueOrOffset }

type ImageFileDirectory =
    { Count : uint16
      Entries : ImageFileEntry seq
      OffsetOfNextImageFileDirectory : uint32 }

type ImageFileHeader =
    { ByteOrder : ByteOrder
      Indicator : Indicator
      OffsetOfFirstImageFileDirectory : uint32 }

type ImageFileElement =
    | FileHeader of ImageFileHeader
    | FileDirectory of ImageFileDirectory
    | Data of byte[]

type ImageFile = ImageFileElement seq

//-------------------------------------------------------------------------------------------------

let private createImageFileEntries width height offsetXResolution offsetYResolution offsetImage sizeImage =

    seq {
        yield { Tag = 262us; Type = 3us; Count = 1u; ValueOrOffset = Value(Short(0us)) }
        yield { Tag = 259us; Type = 3us; Count = 1u; ValueOrOffset = Value(Short(1us)) }
        yield { Tag = 257us; Type = 4us; Count = 1u; ValueOrOffset = Value(Long(height)) }
        yield { Tag = 256us; Type = 4us; Count = 1u; ValueOrOffset = Value(Long(width)) }
        yield { Tag = 296us; Type = 3us; Count = 1u; ValueOrOffset = Value(Short(2us)) }
        yield { Tag = 282us; Type = 5us; Count = 1u; ValueOrOffset = Offset(offsetXResolution) }
        yield { Tag = 283us; Type = 5us; Count = 1u; ValueOrOffset = Offset(offsetYResolution) }
        yield { Tag = 278us; Type = 4us; Count = 1u; ValueOrOffset = Value(Long(height)) }
        yield { Tag = 273us; Type = 4us; Count = 1u; ValueOrOffset = Offset(offsetImage) }
        yield { Tag = 279us; Type = 4us; Count = 1u; ValueOrOffset = Value(Long(sizeImage)) }
    }

let createImageFile width height (resolution : uint32) (bytesImage : byte[]) =

    let bytesResolution = Array.concat [| BitConverter.GetBytes(resolution); BitConverter.GetBytes(1u) |]

    let sizeHeader = uint32 (sizeof<uint16> + sizeof<uint16> + sizeof<uint32>)
    let sizeImage = uint32 bytesImage.Length
    let sizeResolution = uint32 bytesResolution.Length

    let offsetImage = sizeHeader
    let offsetXResolution = offsetImage + sizeImage
    let offsetYResolution = offsetXResolution + sizeResolution
    let offsetDirectories = offsetYResolution + sizeResolution

    let fileEntries = createImageFileEntries width height offsetXResolution offsetYResolution offsetImage sizeImage

    let fileDirectory =
        { Count = uint16 (fileEntries |> Seq.length)
          Entries = fileEntries |> Seq.sortBy (fun x -> x.Tag)
          OffsetOfNextImageFileDirectory = 0u }

    let fileHeader =
        { ByteOrder = ByteOrder.LittleEndian
          Indicator = Indicator.Tiff
          OffsetOfFirstImageFileDirectory = offsetDirectories }

    seq {
        yield FileHeader(fileHeader)
        yield Data(bytesImage)
        yield Data(bytesResolution)
        yield Data(bytesResolution)
        yield FileDirectory(fileDirectory)
    }

//-------------------------------------------------------------------------------------------------

let private serializeImageFileEntry imageFileEntry =

    let serializeValue = function
        | Short(value) -> Array.concat [| BitConverter.GetBytes(value); [| 0uy; 0uy |] |]
        | Long(value) -> BitConverter.GetBytes(value)

    let serializeValueOrOffset = function
        | Value(value) -> value |> serializeValue
        | Offset(value) -> BitConverter.GetBytes(value)

    seq {
        yield BitConverter.GetBytes(imageFileEntry.Tag)
        yield BitConverter.GetBytes(imageFileEntry.Type)
        yield BitConverter.GetBytes(imageFileEntry.Count)
        yield imageFileEntry.ValueOrOffset |> serializeValueOrOffset
    }

let private serializeImageFileDirectory imageFileDirectory =

    seq {
        yield BitConverter.GetBytes(imageFileDirectory.Count)
        yield! imageFileDirectory.Entries |> Seq.collect serializeImageFileEntry
        yield BitConverter.GetBytes(imageFileDirectory.OffsetOfNextImageFileDirectory)
    }

let private serializeImageFileHeader imageFileHeader =

    seq {
        yield BitConverter.GetBytes(uint16 imageFileHeader.ByteOrder)
        yield BitConverter.GetBytes(uint16 imageFileHeader.Indicator)
        yield BitConverter.GetBytes(imageFileHeader.OffsetOfFirstImageFileDirectory)
    }

let serializeImageFile imageFile =

    let serializeImageFileElement = function
        | FileHeader(element) -> element |> serializeImageFileHeader
        | FileDirectory(element) -> element |> serializeImageFileDirectory
        | Data(element) -> seq { yield element }

    let serializeByteChunks chunks =
        use stream = new MemoryStream();
        for bytes in chunks do stream.Write(bytes, 0, bytes.Length)
        stream.ToArray()

    imageFile
    |> Seq.collect serializeImageFileElement
    |> serializeByteChunks
