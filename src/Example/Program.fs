module Example

open System.Drawing
open System.Drawing.Text
open System.IO
open BitonalDocs.Imaging

//-------------------------------------------------------------------------------------------------

let private render (g : Graphics) =

    g.PageUnit <- GraphicsUnit.Inch

    let fontFamily = new FontFamily(GenericFontFamilies.Serif)
    use font = new Font(fontFamily, 12.0f)

    g.DrawString("Hello world!", font, Brushes.Black, 1.0f, 1.0f)

    let penSize = 1.0f / 72.0f
    use pen = new Pen(Brushes.Black, penSize)

    g.DrawLine(pen, 0.25f, 0.25f, 8.25f, 0.25f);
    g.DrawLine(pen, 8.25f, 0.25f, 8.25f, 10.75f);
    g.DrawLine(pen, 8.25f, 10.75f, 0.25f, 10.75f);
    g.DrawLine(pen, 0.25f, 10.75f, 0.25f, 0.25f);

    g.DrawLine(pen, 2.0f, 2.0f, 3.0f, 3.0f);
    g.DrawLine(pen, 3.0f, 2.0f, 2.0f, 3.0f);

[<EntryPoint>]
let main argv =

    let sw = System.Diagnostics.Stopwatch.StartNew()

    let bytes = createTiffImage 8.5f 11.0f 300 render
    File.WriteAllBytes(@"..\..\..\output.tiff", bytes)

    sw.Stop()
    printfn "%i" sw.ElapsedMilliseconds
    0
