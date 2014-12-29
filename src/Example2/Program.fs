module Example2

open System.Drawing
open System.Drawing.Text
open System.IO
open BitonalDocs.Rendering

//-------------------------------------------------------------------------------------------------

let private render (g : Graphics) =

    g.PageUnit <- GraphicsUnit.Inch

    let assembly = System.Reflection.Assembly.GetExecutingAssembly()
    use stream = assembly.GetManifestResourceStream("Lenna.tiff")
    use image = Image.FromStream(stream)

    g.DrawImage(image, Point.Empty)

[<EntryPoint>]
let main argv =

    let sw = System.Diagnostics.Stopwatch.StartNew()

    let bytes = createTiffImage 5.33f 5.33f 96 render
    File.WriteAllBytes(@"..\..\..\output.tiff", bytes)

    sw.Stop()
    printfn "%i" sw.ElapsedMilliseconds
    0
