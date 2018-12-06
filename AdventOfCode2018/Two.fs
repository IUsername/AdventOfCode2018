module Two

let explode (s:string) =
    [for c in s -> c]

let hasPairOrTripletChars (i: string) = 
    let counts = explode i |> Seq.countBy id
    let hasPair = Seq.exists (fun (_,c) -> c = 2) counts
    let hasTriplet = Seq.exists (fun (_,c) -> c = 3) counts
    (hasPair, hasTriplet)

let addCounts (pairs:int, triplets:int) (hasPair:bool, hasTriplet:bool)  =
    match (hasPair, hasTriplet) with
    | (true, true)  -> (pairs + 1, triplets + 1)
    | (true, false) -> (pairs + 1, triplets)
    | (false,true)  -> (pairs,     triplets + 1)
    | (false,false) -> (pairs,     triplets)
    
let rec countAccumulator (ids: seq<string>) =
    let counts = Seq.fold (fun acc id -> hasPairOrTripletChars id |> addCounts acc) (0,0) ids
    fst counts * snd counts

let diffCount (a: seq<char>, b: seq<char>) =
    Seq.zip a b |> Seq.where (fun (ca,cb) -> ca <> cb) |> Seq.length

let seqDifferences (check:string) (ids:seq<string>) = 
    seq {
        let aChars = explode check
        for id in ids do
            let bChars = explode id
            let count = diffCount (aChars, bChars)
            yield (count, check, id)
    }

let rec findMinDiff (ids:string list) (d:int, a:string, b:string) = 
    match ids with 
    | []         -> (d,a,b)
    | head::tail -> match (seqDifferences head tail |> Seq.minBy (fun (d,_,_) -> d)) with
                    | (1, a',b')             -> (1,a',b')
                    | (d',a',b') when d' < d -> findMinDiff tail (d',a',b')
                    | _                      -> findMinDiff tail (d, a, b)

let matchingChars (a: seq<char>) (b: seq<char>) =
    Seq.zip a b |> Seq.where (fun (ca,cb) -> ca = cb) |> Seq.map fst

let implode (xs:seq<char>) = 
    let sb = System.Text.StringBuilder()
    xs |> Seq.iter (sb.Append >> ignore)
    sb.ToString()

type num = int

let commonChars (ids:string list) = 
    let _, a, b = findMinDiff ids (num.MaxValue, "", "")
    matchingChars a b |> implode

let dataSet = @"
ymdrcyapvwfloiuktanxzjsieb
ymdrwhgznwfloiuktanxzjsqeb
ymdrchguvwfloiuktanxmjsleb
pmdrchgmvwfdoiuktanxzjsqeb
ymdrfegpvwfloiukjanxzjsqeb
ymdrchgpvwfloiukmanazjsdeb
ymdsnhgpvwflciuktanxzjsqeb
lmdrbhrpvwfloiuktanxzjsqeb
ymdrwhgpvwfloiukeanxzjsjeb
ymdrchgpvpfloihktanszjsqeb
omdrchgpvwflokuktanazjsqeb
kmsrchgpvwfloiuktanxqjsqeb
ymdrchopvwzloiustanxzjsqeb
omdrchgpvwfloiuktawxtjsqeb
ymdrchgpvwfroiukhanozjsqeb
ymdrchgpvwfloikktanyzosqeb
ymdrchgpvwfioiuktaexzjsqea
ymdrcngpvwfloiuktanxzjsamb
ymdrchgpqwfaoiuktanxxjsqeb
ymdrcmgpvwflziuktakxzjsqeb
ymdrchguvwsloiuktanxzjsqen
ymdrchppowfloiuvtanxzjsqeb
ymdrcngpvwflyiukkanxzjsqeb
ymdrcbgpvwfloiukjanxzjspeb
ymdrchgpvwflopuktanxzosseb
ygdrchgpvwfloiukxanxcjsqeb
ymdrchgpvwfloauktanuzjsqei
ymerchgpvwfloiumtanxzjsqet
ymdlcegpvwfloiuktbnxzjsqeb
ymdrclgpvwfloiukyanxzjlqeb
ymdrchgpvwhmoiuktanxijsqeb
ymdrchgpwrfloiuktanxdjsqeb
ymdbcwgpvwfloiuktanxzusqeb
ymgrchgphwfloiuktanxzjspeb
imdrchgpvwflmiuktanxzjsqib
ymdrihgpvwfloiiktanxzjsteb
ywdrchgpvwfloibkvanxzjsqeb
ymdrchgpxwfloiuktanezjsqes
ymdrchgpiwfloiukxanxzhsqeb
ymdrchgpveflokuktdnxzjsqeb
kmdrchgpvwfloviktanxzjsqeb
ymdrchgpvgfmoiuytanxzjsqeb
ymyrchgpvzfluiuktanxzjsqeb
ymdrchguvwfloiuktanxpjsqlb
ymerchgpvwfloiukthnxsjsqeb
hmdrchgpvwglfiuktanxzjsqeb
ymdrchgpvwfdoiuktanxzjsqaf
yudrchgdvwfloiuktaexzjsqeb
ymdbchgxvwfloiuktanxzjsqem
ymdrchgpvwfloiumjanxzjsqpb
ymdrchgpqwfloiuqtanxrjsqeb
ymdqchhpvwfloiuktanxzzsqeb
ymdryhgpfwfloiuktanxzjsyeb
xmdrchgpvwfloioitanxzjsqeb
ykdrchgpvwfloiuktcnxzisqeb
ymdrcpgprwfloiuktanqzjsqeb
yidrchgpvwfloiuktanxzjgleb
ymdrchgpxwfloiuktanxzjsxfb
ymdrchgfvwfloiuktanxzjiteb
ymdrchgvvwflqifktanxzjsqeb
ymdrchgplwfloiuktanizjnqeb
ymdrchgpvwfyfiuktafxzjsqeb
ymddchgpvwcloiuktanxzjsqeq
ymdrchgkvwflaiuktanxzjsqfb
yudrchgpvwfzoiuktanxzjsreb
ymdrdhgpvwfloiuktnnxqjsqeb
ymdrnhgpvwfloiuktauxzjdqeb
ymdrchgpvwflsiddtanxzjsqeb
ymdrchgpvwhloeuktanxzjsqek
ymdrchgpvjfioiuktawxzjsqeb
ycdrohgpvwfgoiuktanxzjsqeb
ymdrchgpvwflmifktanxzjsqel
yfdrchrpvwfloruktanxzjsqeb
ymdrchgjvwfloiuktanxzrsqeg
ymarchgpxwfloiukkanxzjsqeb
ymdrchgppwflghuktanxzjsqeb
ymdrchvpvwfloiuktanxpjrqeb
ymdlchgpqjfloiuktanxzjsqeb
ymdrchgpvwfofiuktandzjsqeb
ymdrcngpqwfloiuktanlzjsqeb
ymdrchgpvwfloiuiocnxzjsqeb
ymdrcogpvwfloizktanxzjcqeb
ymdrchgpvlfvoiuksanxzjsqeb
ymdrchgpvwflocpctanxzjsqeb
ymdrchgpvwfloiuktanlzjsejb
yndrchgpvwflzifktanxzjsqeb
ymdrcrgpvkfloiuktanxrjsqeb
ymdrchhpvwslocuktanxzjsqeb
ymdrxhgpvwfloiuwtazxzjsqeb
ymdrchgpvafloiuutanxzjsqxb
ymdrchppvhfloquktanxzjsqeb
ymprcugpvwtloiuktanxzjsqeb
ymdrchgpvvflyiuktanxzjsqvb
ymdrchgovwfloiuftanxzjwqeb
ymdrchrpvwflotyktanxzjsqeb
gmdrchgpvwfloauttanxzjsqeb
ymdrchmpvofloiukmanxzjsqeb
ymdrchgpvwflsiuktanxzjspkb
ymdrchgpvwfloluktajxijsqmb
ymdrcngpvwfloiukbanxzdsqeb
ymdrchgpvwploiuktnnxzmsqeb
ymdrcwgpvwfloiuktbnxhjsqeb
ymdrcngpvwfloiuktaaxbjsqeb
ykdrchgpvwfloiuktanxzgsqej
yuhrchgpvwfdoiuktanxzjsqeb
ymdrchgpvsfloiukbanxujsqeb
ymqrchgpvwfliiuktanxzjsteb
ysdqchgpvwfloiuktanxzjtqeb
ymdjchgpcwfloiuktanxzrsqeb
ymdkchgpvwfloiukfanlzjsqeb
ymdrchgpvxfloikktanxzjiqeb
smdrchgwewfloiuktanxzjsqeb
ymdrchgpvwfljiuktanxajsqer
ymdrchgpowflifuktanxzjsqeb
ymdrchgpvpzloiukoanxzjsqeb
yydrchgwvwfvoiuktanxzjsqeb
ymdgcdgpvwflobuktanxzjsqeb
ymdechgpvkfloiuktanxzjsjeb
ymdnchnpvwfloixktanxzjsqeb
ymdrchgpiefloiuktqnxzjsqeb
ymprchgpvwfloiuktjnxzjsxeb
ymdrjdgpzwfloiuktanxzjsqeb
ymsrchgpywfloiuktanxzjsueb
ymdrchgpvgoloiuktanxzcsqeb
ymdrphgpswflbiuktanxzjsqeb
ymqrchgpvnfloiumtanxzjsqeb
ymjrchgpvwyloiuktacxzjsqeb
ymdrchepvwmlqiuktanxzjsqeb
kmirchgpvwfloiuktanxzjsreb
ymdncygpvwfloiuktanuzjsqeb
ymdrzhgpvwploiuktanxzxsqeb
ymdrchkpvwfloiwkmanxzjsqeb
ywdrchgovwfloiuktanxzjsceb
amdrchgpvwfloiuktanrzjqqeb
ymdpshgpvwfloiuktanxzjyqeb
ymdrcegpvwfloijktcnxzjsqeb
ymdrcygpvwfloiuktanxztsqwb
ymdrchgpvufloiuvtabxzjsqeb
ymdrchgpvwflkiuktrnxzjsqmb
ymdrchgpvqfloiuktanxpjfqeb
ymdrclgpvkfloiyktanxzjsqeb
ybdxchgpvwfloiuktanxzjskeb
pmdrchgpvwfzoirktanxzjsqeb
ycdfchgpvwfloiuktanxzjtqeb
ymdrchgpdwfloiumtbnxzjsqeb
ymdrchgpqmfloiuktanxzjsqer
ymgrchgpvwfroiuktanxzjsqey
ymdrnhgpvwfloiuktanjzjsqlb
dmdrchgpvgfloiuktqnxzjsqeb
yudrchgnvwfloiukranxzjsqeb
ymdrxhgpvafloiuktanxzjsqeq
ymdrchgpvwfyofuktanxzjsueb
ymdrrhgpvwfloiuktavxzjsqpb
yvdrchgpvwfloiuktalxzhsqeb
ymdrchgpbwfloiuktanxzfnqeb
ymdrqhgpvwfloiuvtznxzjsqeb
ymdrchgpvbfloiuetanxzjsqeo
ymdrchjpvwfloiuktanxzjnqrb
ymdrchgpmwfqoiuknanxzjsqeb
ymdrchgpvwfuoiuktaqxzjtqeb
ymdrchgpvwfloiuktamxaosqeb
fmdrchgpvffloiuktanxzjsaeb
ymdrrhglvwfwoiuktanxzjsqeb
ymdrchgpvwflohuktanxzjcqei
ymdrcsgpvwfloiuktaexzjsqek
ymlrchfpvwfloiuktpnxzjsqeb
yxdrchgpvwfdoiuvtanxzjsqeb
ymdrchgrvwfloiuktadxzjsqew
ymdrchgpvwbloiyktandzjsqeb
ymdrchgpvsfloiyktanozjsqeb
ymdrchgpjwfloiuktanxibsqeb
ymdrchgjvyfloiuktanxzjsqeh
ymdrchgvvwfloiuktanzrjsqeb
ymdrchgpvwaloiuktynxzjsqev
ymdrccgpvwflonvktanxzjsqeb
ymdrchgqvffloiuktanxfjsqeb
ymdbchgpvwsloiudtanxzjsqeb
ymdachgpvwfloiuktanlzjsqwb
ymdrclgpvwwloiuktanxzjsjeb
ybdpchgpvwdloiuktanxzjsqeb
ymdtchgpvwfleijktanxzjsqeb
ymdrchgpvwfloiustanxzjsxep
ymdrcjypvwfloiuktanxnjsqeb
ymdrcdgpvwfloiuutanxkjsqeb
yhirchgpvufloiuktanxzjsqeb
ymdrlhgpvwfluigktanxzjsqeb
ywdrhhgpvwftoiuktanxzjsqeb
ymdrchgpvwflyiuktanozjsqtb
cmdrchgpuwfloiukmanxzjsqeb
ymdochgpvrfloiuktanvzjsqeb
ymdrcvgpvwfgoiuktfnxzjsqeb
ymdrchgpmufloiuktanxzssqeb
ymurchgrvwfloiuktanxzjsqep
bmdrchgpvwfloiukpanxzjsqmb
ymdrchgphwvloiuktanszjsqeb
ymdpkhgpvwfloiuktanxzjsqtb
ymdrchgpvwfloiuwtanxzjfqev
ymdrchgpvwfloguktqlxzjsqeb
ymkrshgpvwflgiuktanxzjsqeb
ymdrchgpzwfloizktanxznsqeb
ymdrchgpvxfloiuktegxzjsqeb
yydrchgpwwfloiuktanxzjsqqb
ymdrcngwvwfltiuktanxzjsqeb
ymdszhgwvwfloiuktanxzjsqeb
ymdrchguvwfjoiuktanxzxsqeb
ymdomhgpvwfloiuktanxgjsqeb
ymdrcvgpvwfloiuktanwzzsqeb
yydrchgpvwfloiuktanxzjmqtb
rmdrchgpvwfloiuktmnszjsqeb
ykdrchgpvwfloyuktmnxzjsqeb
ymcrchkpvwfloiuktanxzjsoeb
ymdrcrgpvwfloiukpanxzjsceb
yrdrchgpvwfloiukwanxzjsqhb
ymdrcfgpvwfloiurtanxojsqeb
ymdrchgpuwstoiuktanxzjsqeb
ymdrchgpvwflpxuktanxzjsqer
ymdrehgpvwfloiuktabxdjsqeb
yedrchgpvwfloiukqanxzjiqeb
ymdrthgpvyfloiuktanxzjsqen
cmdlchgpvwfloiuvtanxzjsqeb
ymdrchgpvwtloiuktanlpjsqeb
ymdrchgpvwfloiuktanyvjsqea
gmdrcogpvwfloiuktanxzjsqqb
ymmrchgpvwflosuktauxzjsqeb
ymgrchgjvwfloiuktavxzjsqeb
ymdbclgpvwfloeuktanxzjsqeb
ymdrchgpvwfloiuktaixzcsqfb
ymdrchgpvwflmiuktanxttsqeb
ymxrchgpvwfloiuktanxzfsqec
yqzrchgpcwfloiuktanxzjsqeb
yvdrchgpvwfloiukgvnxzjsqeb
ymdrchepvwfloiuktahxzosqeb
ymdlchgpvwfloiuktamizjsqeb
ymdrchgpcwflovuktanxzjsqzb
yvduchgpvwfloiukaanxzjsqeb
ymdrchgpvwfloiuktxmxzjsgeb
ymdrcrgpvwfloizktanbzjsqeb
amdrchgpvwfloiukhanxzjsqbb
ymdrchgpvwfloluktajxijsqeb
ymdrcfgpvwfloiubtanxznsqeb
ymdrchgpvwfleiuwtanxzjsweb
ymdrchgpvwfzdguktanxzjsqeb
ymdrchgwvwflosyktanxzjsqeb
ymrrchgpvwfloiultanxzjsqez
ymdpchgkvwfleiuktanxzjsqeb
ymdrchgpvwfloijktalxfjsqeb
ymdrchgpmwfloiuktanzzjsqfb
ymdrcsgpvwfljiukyanxzjsqeb
ymdrcarpvwfloiuktapxzjsqeb
ymdrchgpvwfloiuktanxzjcqvs
"

