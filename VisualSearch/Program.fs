// Copyright (c) 2012 Francesco De Vittori (http://www.frenk.com)
//
// This code is distributed under the MIT License.
//
// Permission is hereby granted, free of charge, to any person obtaining a
// copy of this software and associated documentation files (the "Software"),
// to deal in the Software without restriction, including without limitation
// the rights to use, copy, modify, merge, publish, distribute, sublicense,
// and/or sell copies of the Software, and to permit persons to whom the
// Software is furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included
// in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
// OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
// DEALINGS IN THE SOFTWARE.


open System.IO
open System.Windows.Media
open System.Windows.Media.Imaging

type Histogram = {
    Data : float array
    FileName : string
}


/// Returns the pixels of an image as a byte array,
/// in form [Alpha, B, G, R, Alpha, B, G, R, ...]
let getPixels imgPath =
    try
        let s = new BitmapImage (new System.Uri (imgPath))
        let source =
            if s.Format <> PixelFormats.Bgr32
            then new FormatConvertedBitmap (s, PixelFormats.Bgr32, null, 0.)
                 :> BitmapSource
            else s :> BitmapSource

        let width = source.PixelWidth
        let height = source.PixelHeight    

        let pixels = Array.create (width * height * 4) 0uy
        source.CopyPixels (pixels, width * 4, 0)
        Some pixels
    with
    | :? System.NotSupportedException -> None


/// Combines the given R, G, B values (8 bits each)
/// into a single byte, discarding the least significant
/// bits.
let to8bpp red green blue =
    let b = blue >>> 6
    let g = green >>> 5
    let r = red >>> 5
    0uy ||| (r <<< 5) ||| (g <<< 2) ||| b


/// Converts 32-bits ABGR to 8-bits "truecolor": 2 bits for blue,
/// 3 for green, 3 for red (RGB).
/// Expects an array in form [ Alpha, B, G, R, Alpha, B, G, R, ... ]
/// Returns an array of pixels where each pixel is a byte (RGB).
let to8bit px32bpp =
    [| for i in 0..4..((Array.length px32bpp) - 4) ->
        to8bpp px32bpp.[i + 2] px32bpp.[i + 1] px32bpp.[i]
    |]


/// Creates the normalized color distribution from
/// an image. Returns a 256-color histogram.
let makeHistogram (fileName : string) =

    getPixels fileName
    |> Option.bind (fun pxs ->
        let pixels8 = to8bit pxs

        // creates an empty histogram
        let histogram = Array.create 256 0.
        let pixelCount = Array.length pixels8

        // counts the number of occurrences of every color
        for i in 0..pixelCount - 1 do
            let color = int pixels8.[i]
            histogram.[color] <- histogram.[color] + 1.

        // normalizes the histogram
        let normalized = [| for i in 0..histogram.Length - 1 ->
                            System.Math.Round(histogram.[i] / float pixelCount, 4)
                         |]

        // returns the image "signature"
        Some { Histogram.Data = normalized; FileName = fileName }
      )


let KullbackLeiblerDist p q =
    Array.map2 (fun pi qi -> if pi = 0.
                             then 0.
                             else pi * log (pi / qi))
               p q
    |> Array.sum

let JensenShannonDist p q =
    let m = Array.map2 (fun pi qi -> (pi + qi) / 2.)
                       p q
    (KullbackLeiblerDist p m) / 2. + (KullbackLeiblerDist q m) / 2.

let quadFormDistance (hist1 : float array) (hist2 : float array) =
        Array.map2 (fun pi qi ->
                        if pi = 0. && qi = 0.
                        then 0.
                        else 0.5 * (pown (pi - qi) 2) / (pi + qi))
                   hist1
                   hist2
        |> Array.sum

let distance p q =
    sqrt (JensenShannonDist p q)

/// Returns the histogram closest to the given one.
let nearestNeighbor sample samples =
    samples
    |> Seq.map (fun s -> (s.FileName, distance s.Data sample.Data))
    |> Seq.sortBy (fun (fn, dist) -> dist)
    |> Seq.head
    |> fst


[<EntryPointAttribute>]
let main args =
    let query = "c:\\me.jpg"
    let images = "c:\\myImages"

    if not (Directory.Exists images)
    then invalidOp "The samples directory does not exist."

    if not (File.Exists query)
    then invalidOp "The query image does not exist."

    let queryImage = match makeHistogram query with
                     | Some img -> img
                     | None -> invalidOp "The query image could not be loaded."

    let sampleImages =
        Directory.GetFiles (images, "*.jpg")
        |> Array.Parallel.map makeHistogram
        |> Array.filter Option.isSome
        |> Array.map Option.get

    if (Array.length sampleImages) = 0
    then invalidOp "The samples directory does not contain any valid image."

    let mostSimilar = nearestNeighbor queryImage sampleImages

    System.Console.WriteLine
        (System.String.Format
            ("The most similar image is '{0}'", mostSimilar))

    ignore (System.Console.ReadLine ())
    0