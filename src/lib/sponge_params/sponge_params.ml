[%%import
"../../config.mlh"]

open Curve_choice.Tick0

[%%if
curve_size = 298]

let inv_alpha =
  "432656623790237568866681136048225865041022616866203195957516123399240588461280445963602851"

let mds =
  [| [| Field.of_string
          "181324588122329172048070802614406344967661900669343676997796156524662650229663511778086513"
      ; Field.of_string
          "263839662794798230944406038483748877420003467481254943330033497627810628977768312588897021"
      ; Field.of_string
          "47787034086054868794736504598805355240746067397315425760363325479582067585554122384528750"
     |]
   ; [| Field.of_string
          "391385728862913577230643656405794210023251219169789744235774373121108965138889307827345939"
      ; Field.of_string
          "368056256556859616791833365938123080683505948787537081082804782658777406001515743364112843"
      ; Field.of_string
          "249229689710372851346889167834108105226843437678081232334602983010385341756350839066179566"
     |]
   ; [| Field.of_string
          "391761630355250451965959916078641131603140945583687294349414005799846378806556028223600720"
      ; Field.of_string
          "309426222273897994989187985039896323914733463925481353595665936771905869408957537639744345"
      ; Field.of_string
          "429282034891350663871556405902853196474768911490694799502975387461169986038745882893853806"
     |] |]

let round_constants =
  [| [| Field.of_string
          "78119860594733808983474265082430117124674905785489385612351809573030163625517"
      ; Field.of_string
          "41917899842730241418346215913324270532073353586134123463219061327941260175271"
      ; Field.of_string
          "74594641694171623328644944059182600919855574964222988275913344198970402906473"
     |]
   ; [| Field.of_string
          "96215759378377024990520153908983544755208851791126218239402755616994541522004"
      ; Field.of_string
          "64070601581278917442704840630680311036021557676765751754522901046069205253111"
      ; Field.of_string
          "112123228532462696722378911494343451272980413618911326680094528285518792872677"
     |]
   ; [| Field.of_string
          "84572244072021308337360477634782636535511175281144388234379224309078196768262"
      ; Field.of_string
          "45201095631123410354816854701250642083197167601967427301389500806815426216645"
      ; Field.of_string
          "23419302413627434057960523568681421397183896397903197013759822219271473949448"
     |]
   ; [| Field.of_string
          "63220724218126871510891512179599337793645245415246618202146262033908228783613"
      ; Field.of_string
          "67900966560828272306360950341997532094196196655192755442359232962244590070115"
      ; Field.of_string
          "56382132371728071364028077587343004835658613510701494793375685201885283260755"
     |]
   ; [| Field.of_string
          "80317852656339951095312898663286716255545986714650554749917139819628941702909"
      ; Field.of_string
          "110977183257428423540294096816813859894739618561444416996538397449475628658639"
      ; Field.of_string
          "25195781166503180938390820610484311038421647727795615447439501669639084690800"
     |]
   ; [| Field.of_string
          "108664438541952156416331885221418851366456449596370568350972106298760717710264"
      ; Field.of_string
          "17649294376560630922417546944777537620537408190408066211453084495108565929366"
      ; Field.of_string
          "95236435002924956844837407534938226368352771792739587594037613075251645052212"
     |]
   ; [| Field.of_string
          "43150472723422600689013423057826322506171125106415122422656432973040257528684"
      ; Field.of_string
          "77355911134402286174761911573353899889837132781450260391484427670446862700214"
      ; Field.of_string
          "8690728446593494554377477996892461126663797704587025899930929227865493269824"
     |]
   ; [| Field.of_string
          "109175231986025180460846040078523879514558355792739714578031829643740609438879"
      ; Field.of_string
          "64844253590731404811389281562033735091759746904073461140427127388042062490899"
      ; Field.of_string
          "43237071281695629980341250188156848876595681601471702180515324064382368960951"
     |]
   ; [| Field.of_string
          "2704440995725305992776846806711930876273040749514871232837487081811513368296"
      ; Field.of_string
          "66806779110388532101035294912010606217442229808784290357894909707660045365269"
      ; Field.of_string
          "25541187612624070470730890200174075890643652797181103367956318438136878170352"
     |]
   ; [| Field.of_string
          "89300613074831725721350087269266903129165086877175223066581882601662278010666"
      ; Field.of_string
          "36824076981866281177052433916337787028520068526782493484076995129329938182524"
      ; Field.of_string
          "68880449342008497744225106025198236600142055580985632884415488154606462819445"
     |]
   ; [| Field.of_string
          "68556888546596545408135887526582256648006271867854316538090068824142539400698"
      ; Field.of_string
          "111379753250206255125320675615931203940253796355491142745969887430259465111569"
      ; Field.of_string
          "101469186248899356416491489235841069222521093012237305521090058066171355672289"
     |]
   ; [| Field.of_string
          "87819793263125973233157093200229218382531712066157093399606059493857616731410"
      ; Field.of_string
          "11055386921184594780372263378420826851562920740321950336882051897732501262543"
      ; Field.of_string
          "111945832089295501567161822264292548657346358707472584179854375613919325491249"
     |]
   ; [| Field.of_string
          "95630018375719472826904441325138673248990446382783206900295723762884876505178"
      ; Field.of_string
          "94833984285990985873155989049880754188702918168949640563745233736765833491756"
      ; Field.of_string
          "77578854197021606645372788474039811639438242484066959482386065023999206730771"
     |]
   ; [| Field.of_string
          "27799616729223271646690718201487403976485619375555391888533887467404804041014"
      ; Field.of_string
          "42616502170265664498961018686434252976977548128285781725227341660941880774718"
      ; Field.of_string
          "95884094505080541517768389956970969462501217028562326732054532092615835087122"
     |]
   ; [| Field.of_string
          "107531500891040898338150732759493933154418374543568088749403053559827078391994"
      ; Field.of_string
          "17316158269457914256007584527534747738658973027567786054549020564540952112346"
      ; Field.of_string
          "51624680144452294805663893795879183520785046924484587034566439599591446246116"
     |]
   ; [| Field.of_string
          "17698087730709566968258013675219881840614043344609152682517330801348583470562"
      ; Field.of_string
          "111925747861248746962567200879629070277886617811519137515553806421564944666811"
      ; Field.of_string
          "57148554624730554436721083599187229462914514696466218614205595953570212881615"
     |]
   ; [| Field.of_string
          "92002976914130835490768248031171915767210477082066266868807636677032557847243"
      ; Field.of_string
          "58807951133460826577955909810426403194149348045831674376120801431489918282349"
      ; Field.of_string
          "93581873597000319446791963913210464830992618681307774190204379970955657554666"
     |]
   ; [| Field.of_string
          "46734218328816451470118898692627799522173317355773128175090189234250221977353"
      ; Field.of_string
          "12565476532112137808460978474958060441970941349010371267577877299656634907765"
      ; Field.of_string
          "54284813390357004119220859882274190703294683700710665367594256039714984623777"
     |]
   ; [| Field.of_string
          "92046423253202913319296401122133532555630886766139313429473309376931112550800"
      ; Field.of_string
          "15095408309586969968044201398966210357547906905122453139947200130015688526573"
      ; Field.of_string
          "76483858663950700865536712701042004661599554591777656961315837882956812689085"
     |]
   ; [| Field.of_string
          "37793510665854947576525000802927849210746292216845467892500370179796223909690"
      ; Field.of_string
          "84954934523349224038508216623641462700694917568481430996824733443763638196693"
      ; Field.of_string
          "81116649005575743294029244339854405387811058321603450814032274416116019472096"
     |]
   ; [| Field.of_string
          "28313841745366368076212445154871968929195537523489133192784916081223753077949"
      ; Field.of_string
          "17307716513182567320564075539526480893558355908652993731441220999922946005081"
      ; Field.of_string
          "63148771170858502457695904149048034226689843239981287723002468627916462842625"
     |]
   ; [| Field.of_string
          "14724939606645168531546334343600232253284320276481307778787768813885931648950"
      ; Field.of_string
          "4684996260500305121238590806572541849891754312215139285622888510153705963000"
      ; Field.of_string
          "63682763879011752475568476861367553456179860221069473817315669232908763409259"
     |]
   ; [| Field.of_string
          "47776179656187399887062096850541192680190218704758942820514561435612697426715"
      ; Field.of_string
          "42017618175533328439486588850450028995049195954365035474995309904751824054581"
      ; Field.of_string
          "39169739448648613641258102792190571431737464735838931948313779997907435855102"
     |]
   ; [| Field.of_string
          "37525991163523321662699819448962967746703579202577998445997476955224037837979"
      ; Field.of_string
          "67759173441312327668891803222741396828094999063019622301649400178376863820046"
      ; Field.of_string
          "23041132473771739182071223620364590606653086905326129708428084432335332411661"
     |]
   ; [| Field.of_string
          "77778894465896892167598828497939467663479992533052348475467490972714790615441"
      ; Field.of_string
          "20821227542001445006023346122554483849065713580779858784021328359824080462519"
      ; Field.of_string
          "47217242463811495777303984778653549585537750303740616187093690846833142245039"
     |]
   ; [| Field.of_string
          "42826871300142174590405062658305130206548405024021455479047593769907201224399"
      ; Field.of_string
          "8850081254230234130482383430433176873344633494243110112848647064077741649744"
      ; Field.of_string
          "1819639941546179668398979507053724449231350395599747300736218202072168364980"
     |]
   ; [| Field.of_string
          "21219092773772827667886204262476112905428217689703647484316763603169544906986"
      ; Field.of_string
          "35036730416829620763976972888493029852952403098232484869595671405553221294746"
      ; Field.of_string
          "35487050610902505183766069070898136230610758743267437784506875078109148276407"
     |]
   ; [| Field.of_string
          "62560813042054697786535634928462520639989597995560367915904328183428481834648"
      ; Field.of_string
          "112205708104999693686115882430330200785082630634036862526175634736046083007596"
      ; Field.of_string
          "109084747126382177842005646092084591250172358815974554434100716599544229364287"
     |]
   ; [| Field.of_string
          "63740884245554590221521941789197287379354311786803164550686696984009448418872"
      ; Field.of_string
          "58779928727649398559174292364061339806256990859940639552881479945324304668069"
      ; Field.of_string
          "20614241966717622390914334053622572167995367802051836931454426877074875942253"
     |]
   ; [| Field.of_string
          "41621411615229558798583846330993607380846912281220890296433013153854774573504"
      ; Field.of_string
          "20530621481603446397085836296967350209890164029268319619481535419199429275412"
      ; Field.of_string
          "99914592017824500091708233310179001698739309503141229228952777264267035511439"
     |]
   ; [| Field.of_string
          "9497854724940806346676139162466690071592872530638144182764466319052293463165"
      ; Field.of_string
          "7549205476288061047040852944548942878112823732145584918107208536541712726277"
      ; Field.of_string
          "30898915730863004722886730649661235919513859500318540107289237568593577554645"
     |]
   ; [| Field.of_string
          "22697249754607337581727259086359907309326296469394183645633378468855554942575"
      ; Field.of_string
          "72771100592475003378969523202338527077495914171905204927442739996373603143216"
      ; Field.of_string
          "84509851995167666169868678185342549983568150803791023831909660012392522615426"
     |]
   ; [| Field.of_string
          "36601166816771446688370845080961015541431660429079281633209182736773260407536"
      ; Field.of_string
          "19555759172327736128240171000715903945570888389700763573790859521156095228287"
      ; Field.of_string
          "82844424532983875300577689116331373756526403900340445449185486212503235782229"
     |]
   ; [| Field.of_string
          "40833119728631657038301474658571416779079199343770917422783737091842927892625"
      ; Field.of_string
          "68922359316478675184342553333343300163568193749010867527082189412217781430311"
      ; Field.of_string
          "91516472400306837063911995909475588197278444979245081960087094196120449075833"
     |]
   ; [| Field.of_string
          "21304716730402869084944080869903443431235336418077153507261240151959530377653"
      ; Field.of_string
          "106551237424345741137570659736231801772439680702621554106791455938098031620471"
      ; Field.of_string
          "104392597313271110590927764888829150750277653499050463757708547416538850601163"
     |]
   ; [| Field.of_string
          "16907937154215020261110468963982390213438461071031811101554056252102505124726"
      ; Field.of_string
          "23183141532591565112222057191012766855134687114504142337903677590107533245206"
      ; Field.of_string
          "96725517880771645283128624101279195709280644465575982072053504613644938879246"
     |]
   ; [| Field.of_string
          "84556507395241990875812091718422997082915179448604219593521819129312718969906"
      ; Field.of_string
          "100646525819453650494590571397259055384579251368754179569362740802641255820576"
      ; Field.of_string
          "50316555026297423940834952362583934362215303629664094841692233643882339493043"
     |]
   ; [| Field.of_string
          "77363534410783423412630139556441807611393685349073113946053979350631229049878"
      ; Field.of_string
          "54905073434434959485893381841839373267383966385817882684657825178181863944371"
      ; Field.of_string
          "110016011331508430102821620395154714608084938556260733745010992614542669817451"
     |]
   ; [| Field.of_string
          "52040139270046094723964229965823921970388683619580004402190656733318120479093"
      ; Field.of_string
          "495546618036723566920914648951352373868059898268055487677897567226892784967"
      ; Field.of_string
          "2528292188392170914010448139211586215817069915670005292953294092269979070980"
     |]
   ; [| Field.of_string
          "36842840134449713950999812540127591123318806680559982063089906871196226758113"
      ; Field.of_string
          "112314504940338253416202605695368724580971154020421327790335219348068041886245"
      ; Field.of_string
          "51653712314537383078368021242008468828072907802445786549975419682333073143987"
     |]
   ; [| Field.of_string
          "27179054135131403873076215577181710354069071017096145081169516607932870071868"
      ; Field.of_string
          "93264325401956094073193527739715293258814405715822269809955952297346626219055"
      ; Field.of_string
          "75336695567377817226085396912086909560962335091652231383627608374094112503635"
     |]
   ; [| Field.of_string
          "42536477740858058164730818130587261149155820207748153094480456895727052896150"
      ; Field.of_string
          "45297707210835305388426482743535401273114010430724989418303851665124351001731"
      ; Field.of_string
          "28263543670875633354854018109712021307749750769690268127459707194207091046997"
     |]
   ; [| Field.of_string
          "40809484989590048522440442751358616303471639779690405026946053699354967624695"
      ; Field.of_string
          "51589519265418587649124543325590658874910911006853535317847189422703251228717"
      ; Field.of_string
          "73459936981642894525955700397592343967482441686326322443228255968694436816673"
     |]
   ; [| Field.of_string
          "87298777232393189731949522229743081866971743270330772607820990832164835738703"
      ; Field.of_string
          "23328534428894097247289332213412175849711532153957647506361455182140450133738"
      ; Field.of_string
          "51807348624578081645565456865744011145427112815128832643950401419083788780028"
     |]
   ; [| Field.of_string
          "62003629107726929116302469001779155132709624140360743951550189738290955064278"
      ; Field.of_string
          "109311858027068383034683875948676795998030610067675200794951297783857157095297"
      ; Field.of_string
          "2085588517087605436136379278738013214233743532079287631079316773925068862732"
     |]
   ; [| Field.of_string
          "9513664655545306376987968929852776467090105742275395185801917554996684570014"
      ; Field.of_string
          "91103467624252027317764670613760419385374004736848754250298970998535616755199"
      ; Field.of_string
          "39500000352127197728032684892425352332461947514533659433380855624868454474623"
     |]
   ; [| Field.of_string
          "75175260486328125629270378861920310368403601365269629778076078053196928460032"
      ; Field.of_string
          "56923881233337629517433981230592855430598464522180216309153828833928801967999"
      ; Field.of_string
          "20981004218820236011689230170078809973840534961691702543937445515733151438851"
     |]
   ; [| Field.of_string
          "73175203586574092105626230272409823792532423094740797516874387144340145138310"
      ; Field.of_string
          "45186992623753580336479418079070607289916086076906975839720879934817804495460"
      ; Field.of_string
          "96084125187548549854900995260973117424750860440064269432639526863495781270780"
     |]
   ; [| Field.of_string
          "53530507055579550362119832302266967544350117012822630711681736383163390079758"
      ; Field.of_string
          "24484677147631687826970700541691541659768738376645174313438582486313045584324"
      ; Field.of_string
          "99915577684197600584703320523786830947563355229812244982453188909016758004559"
     |]
   ; [| Field.of_string
          "73101441225016284181831039876112223954723401962484828024235461623078642642543"
      ; Field.of_string
          "57434882751817972247799186935032874577110609253567900895922769490031350316077"
      ; Field.of_string
          "73837027842771758252813592393497967898989365991569964687267097531033696791279"
     |] |]

[%%elif
curve_size = 753]

let inv_alpha =
  "38089537243562684911222013446582397389246099927230862792530457200932138920519187975508085239809399019470973610807689524839248234083267140972451128958905814696110378477590967674064016488951271336010850653690825603837076796509091"

let mds =
  [| [| Field.of_string
          "18008368437737423474309001369890301521976028259557869102888851965525650962978826556079921598599098888590302388431866694591858505845787597588918688371226882207991627422083815074127761663891796146172734531991290402968541914191945"
      ; Field.of_string
          "32962087054539410523956712909309686802653898657605569239066684150412875533806729129396719808139132458477579312916467576544007112173179883702872518317566248974424872120316787037296877442303550788674087649228607529914336317231815"
      ; Field.of_string
          "5483644920564955035638567475101171013329909513705951195576914157062781400017095978578204379959018576768230785151221956162299596069033091214145892295417145700700562355150808732841416210677611704678816316579070697592848376918151"
     |]
   ; [| Field.of_string
          "22978648816866328436434244623482365207916416489037627250857600725663194263360344221738155318310265722276036466391561221273100146793047089336717612168000266025808046352571957200240941276891050003938106626328014785436301089444973"
      ; Field.of_string
          "30994637628885441247541289546067547358628828834593234742882770745561956454298316691254641971835514862825457645395555821312465912408960063865618013131940007283956832467402859348036195396599351702172170219903104023278420827940135"
      ; Field.of_string
          "7096546890972108774287040498267941446510912236116268882520023333699636048386130304511472040490894498194252489942856762189629237475878134498814298584446894911200379613916180563419809701971057277837757006070684068787238347669992"
     |]
   ; [| Field.of_string
          "36972350749469737754741804679554799140755989986720531577443294433161553396641362942311484418395414339763390349161399190591697773588979458652577643792428305947365748981633559835484411429153283032734484874265223184021528054671667"
      ; Field.of_string
          "41737243523985324129413602960234190443256387558212939183466624464606481865667576817406507424236723364751044981130064473555650490691461017936143464747647507236853158008794221676669840197156981325463879378696484711828785706949884"
      ; Field.of_string
          "17173689835840458026597473076649786448044751322360472626284380020090825232350642484368920024327948574743378803111953285570783101340571478325610471380479472689631139762331626281838772360396878623880994496993923849428256427219637"
     |] |]

let round_constants =
  [| [| Field.of_string
          "78119860594733808983474265082430117124674905785489385612351809573030163625517"
      ; Field.of_string
          "41917899842730241418346215913324270532073353586134123463219061327941260175271"
      ; Field.of_string
          "74594641694171623328644944059182600919855574964222988275913344198970402906473"
     |]
   ; [| Field.of_string
          "96215759378377024990520153908983544755208851791126218239402755616994541522004"
      ; Field.of_string
          "64070601581278917442704840630680311036021557676765751754522901046069205253111"
      ; Field.of_string
          "112123228532462696722378911494343451272980413618911326680094528285518792872677"
     |]
   ; [| Field.of_string
          "84572244072021308337360477634782636535511175281144388234379224309078196768262"
      ; Field.of_string
          "45201095631123410354816854701250642083197167601967427301389500806815426216645"
      ; Field.of_string
          "23419302413627434057960523568681421397183896397903197013759822219271473949448"
     |]
   ; [| Field.of_string
          "63220724218126871510891512179599337793645245415246618202146262033908228783613"
      ; Field.of_string
          "67900966560828272306360950341997532094196196655192755442359232962244590070115"
      ; Field.of_string
          "56382132371728071364028077587343004835658613510701494793375685201885283260755"
     |]
   ; [| Field.of_string
          "80317852656339951095312898663286716255545986714650554749917139819628941702909"
      ; Field.of_string
          "110977183257428423540294096816813859894739618561444416996538397449475628658639"
      ; Field.of_string
          "25195781166503180938390820610484311038421647727795615447439501669639084690800"
     |]
   ; [| Field.of_string
          "108664438541952156416331885221418851366456449596370568350972106298760717710264"
      ; Field.of_string
          "17649294376560630922417546944777537620537408190408066211453084495108565929366"
      ; Field.of_string
          "95236435002924956844837407534938226368352771792739587594037613075251645052212"
     |]
   ; [| Field.of_string
          "43150472723422600689013423057826322506171125106415122422656432973040257528684"
      ; Field.of_string
          "77355911134402286174761911573353899889837132781450260391484427670446862700214"
      ; Field.of_string
          "8690728446593494554377477996892461126663797704587025899930929227865493269824"
     |]
   ; [| Field.of_string
          "109175231986025180460846040078523879514558355792739714578031829643740609438879"
      ; Field.of_string
          "64844253590731404811389281562033735091759746904073461140427127388042062490899"
      ; Field.of_string
          "43237071281695629980341250188156848876595681601471702180515324064382368960951"
     |]
   ; [| Field.of_string
          "2704440995725305992776846806711930876273040749514871232837487081811513368296"
      ; Field.of_string
          "66806779110388532101035294912010606217442229808784290357894909707660045365269"
      ; Field.of_string
          "25541187612624070470730890200174075890643652797181103367956318438136878170352"
     |]
   ; [| Field.of_string
          "89300613074831725721350087269266903129165086877175223066581882601662278010666"
      ; Field.of_string
          "36824076981866281177052433916337787028520068526782493484076995129329938182524"
      ; Field.of_string
          "68880449342008497744225106025198236600142055580985632884415488154606462819445"
     |]
   ; [| Field.of_string
          "68556888546596545408135887526582256648006271867854316538090068824142539400698"
      ; Field.of_string
          "111379753250206255125320675615931203940253796355491142745969887430259465111569"
      ; Field.of_string
          "101469186248899356416491489235841069222521093012237305521090058066171355672289"
     |]
   ; [| Field.of_string
          "87819793263125973233157093200229218382531712066157093399606059493857616731410"
      ; Field.of_string
          "11055386921184594780372263378420826851562920740321950336882051897732501262543"
      ; Field.of_string
          "111945832089295501567161822264292548657346358707472584179854375613919325491249"
     |]
   ; [| Field.of_string
          "95630018375719472826904441325138673248990446382783206900295723762884876505178"
      ; Field.of_string
          "94833984285990985873155989049880754188702918168949640563745233736765833491756"
      ; Field.of_string
          "77578854197021606645372788474039811639438242484066959482386065023999206730771"
     |]
   ; [| Field.of_string
          "27799616729223271646690718201487403976485619375555391888533887467404804041014"
      ; Field.of_string
          "42616502170265664498961018686434252976977548128285781725227341660941880774718"
      ; Field.of_string
          "95884094505080541517768389956970969462501217028562326732054532092615835087122"
     |]
   ; [| Field.of_string
          "107531500891040898338150732759493933154418374543568088749403053559827078391994"
      ; Field.of_string
          "17316158269457914256007584527534747738658973027567786054549020564540952112346"
      ; Field.of_string
          "51624680144452294805663893795879183520785046924484587034566439599591446246116"
     |]
   ; [| Field.of_string
          "17698087730709566968258013675219881840614043344609152682517330801348583470562"
      ; Field.of_string
          "111925747861248746962567200879629070277886617811519137515553806421564944666811"
      ; Field.of_string
          "57148554624730554436721083599187229462914514696466218614205595953570212881615"
     |]
   ; [| Field.of_string
          "92002976914130835490768248031171915767210477082066266868807636677032557847243"
      ; Field.of_string
          "58807951133460826577955909810426403194149348045831674376120801431489918282349"
      ; Field.of_string
          "93581873597000319446791963913210464830992618681307774190204379970955657554666"
     |]
   ; [| Field.of_string
          "46734218328816451470118898692627799522173317355773128175090189234250221977353"
      ; Field.of_string
          "12565476532112137808460978474958060441970941349010371267577877299656634907765"
      ; Field.of_string
          "54284813390357004119220859882274190703294683700710665367594256039714984623777"
     |]
   ; [| Field.of_string
          "92046423253202913319296401122133532555630886766139313429473309376931112550800"
      ; Field.of_string
          "15095408309586969968044201398966210357547906905122453139947200130015688526573"
      ; Field.of_string
          "76483858663950700865536712701042004661599554591777656961315837882956812689085"
     |]
   ; [| Field.of_string
          "37793510665854947576525000802927849210746292216845467892500370179796223909690"
      ; Field.of_string
          "84954934523349224038508216623641462700694917568481430996824733443763638196693"
      ; Field.of_string
          "81116649005575743294029244339854405387811058321603450814032274416116019472096"
     |]
   ; [| Field.of_string
          "28313841745366368076212445154871968929195537523489133192784916081223753077949"
      ; Field.of_string
          "17307716513182567320564075539526480893558355908652993731441220999922946005081"
      ; Field.of_string
          "63148771170858502457695904149048034226689843239981287723002468627916462842625"
     |]
   ; [| Field.of_string
          "14724939606645168531546334343600232253284320276481307778787768813885931648950"
      ; Field.of_string
          "4684996260500305121238590806572541849891754312215139285622888510153705963000"
      ; Field.of_string
          "63682763879011752475568476861367553456179860221069473817315669232908763409259"
     |]
   ; [| Field.of_string
          "47776179656187399887062096850541192680190218704758942820514561435612697426715"
      ; Field.of_string
          "42017618175533328439486588850450028995049195954365035474995309904751824054581"
      ; Field.of_string
          "39169739448648613641258102792190571431737464735838931948313779997907435855102"
     |]
   ; [| Field.of_string
          "37525991163523321662699819448962967746703579202577998445997476955224037837979"
      ; Field.of_string
          "67759173441312327668891803222741396828094999063019622301649400178376863820046"
      ; Field.of_string
          "23041132473771739182071223620364590606653086905326129708428084432335332411661"
     |]
   ; [| Field.of_string
          "77778894465896892167598828497939467663479992533052348475467490972714790615441"
      ; Field.of_string
          "20821227542001445006023346122554483849065713580779858784021328359824080462519"
      ; Field.of_string
          "47217242463811495777303984778653549585537750303740616187093690846833142245039"
     |]
   ; [| Field.of_string
          "42826871300142174590405062658305130206548405024021455479047593769907201224399"
      ; Field.of_string
          "8850081254230234130482383430433176873344633494243110112848647064077741649744"
      ; Field.of_string
          "1819639941546179668398979507053724449231350395599747300736218202072168364980"
     |]
   ; [| Field.of_string
          "21219092773772827667886204262476112905428217689703647484316763603169544906986"
      ; Field.of_string
          "35036730416829620763976972888493029852952403098232484869595671405553221294746"
      ; Field.of_string
          "35487050610902505183766069070898136230610758743267437784506875078109148276407"
     |]
   ; [| Field.of_string
          "62560813042054697786535634928462520639989597995560367915904328183428481834648"
      ; Field.of_string
          "112205708104999693686115882430330200785082630634036862526175634736046083007596"
      ; Field.of_string
          "109084747126382177842005646092084591250172358815974554434100716599544229364287"
     |]
   ; [| Field.of_string
          "63740884245554590221521941789197287379354311786803164550686696984009448418872"
      ; Field.of_string
          "58779928727649398559174292364061339806256990859940639552881479945324304668069"
      ; Field.of_string
          "20614241966717622390914334053622572167995367802051836931454426877074875942253"
     |]
   ; [| Field.of_string
          "41621411615229558798583846330993607380846912281220890296433013153854774573504"
      ; Field.of_string
          "20530621481603446397085836296967350209890164029268319619481535419199429275412"
      ; Field.of_string
          "99914592017824500091708233310179001698739309503141229228952777264267035511439"
     |]
   ; [| Field.of_string
          "9497854724940806346676139162466690071592872530638144182764466319052293463165"
      ; Field.of_string
          "7549205476288061047040852944548942878112823732145584918107208536541712726277"
      ; Field.of_string
          "30898915730863004722886730649661235919513859500318540107289237568593577554645"
     |]
   ; [| Field.of_string
          "22697249754607337581727259086359907309326296469394183645633378468855554942575"
      ; Field.of_string
          "72771100592475003378969523202338527077495914171905204927442739996373603143216"
      ; Field.of_string
          "84509851995167666169868678185342549983568150803791023831909660012392522615426"
     |]
   ; [| Field.of_string
          "36601166816771446688370845080961015541431660429079281633209182736773260407536"
      ; Field.of_string
          "19555759172327736128240171000715903945570888389700763573790859521156095228287"
      ; Field.of_string
          "82844424532983875300577689116331373756526403900340445449185486212503235782229"
     |]
   ; [| Field.of_string
          "40833119728631657038301474658571416779079199343770917422783737091842927892625"
      ; Field.of_string
          "68922359316478675184342553333343300163568193749010867527082189412217781430311"
      ; Field.of_string
          "91516472400306837063911995909475588197278444979245081960087094196120449075833"
     |]
   ; [| Field.of_string
          "21304716730402869084944080869903443431235336418077153507261240151959530377653"
      ; Field.of_string
          "106551237424345741137570659736231801772439680702621554106791455938098031620471"
      ; Field.of_string
          "104392597313271110590927764888829150750277653499050463757708547416538850601163"
     |]
   ; [| Field.of_string
          "16907937154215020261110468963982390213438461071031811101554056252102505124726"
      ; Field.of_string
          "23183141532591565112222057191012766855134687114504142337903677590107533245206"
      ; Field.of_string
          "96725517880771645283128624101279195709280644465575982072053504613644938879246"
     |]
   ; [| Field.of_string
          "84556507395241990875812091718422997082915179448604219593521819129312718969906"
      ; Field.of_string
          "100646525819453650494590571397259055384579251368754179569362740802641255820576"
      ; Field.of_string
          "50316555026297423940834952362583934362215303629664094841692233643882339493043"
     |]
   ; [| Field.of_string
          "77363534410783423412630139556441807611393685349073113946053979350631229049878"
      ; Field.of_string
          "54905073434434959485893381841839373267383966385817882684657825178181863944371"
      ; Field.of_string
          "110016011331508430102821620395154714608084938556260733745010992614542669817451"
     |]
   ; [| Field.of_string
          "52040139270046094723964229965823921970388683619580004402190656733318120479093"
      ; Field.of_string
          "495546618036723566920914648951352373868059898268055487677897567226892784967"
      ; Field.of_string
          "2528292188392170914010448139211586215817069915670005292953294092269979070980"
     |]
   ; [| Field.of_string
          "36842840134449713950999812540127591123318806680559982063089906871196226758113"
      ; Field.of_string
          "112314504940338253416202605695368724580971154020421327790335219348068041886245"
      ; Field.of_string
          "51653712314537383078368021242008468828072907802445786549975419682333073143987"
     |]
   ; [| Field.of_string
          "27179054135131403873076215577181710354069071017096145081169516607932870071868"
      ; Field.of_string
          "93264325401956094073193527739715293258814405715822269809955952297346626219055"
      ; Field.of_string
          "75336695567377817226085396912086909560962335091652231383627608374094112503635"
     |]
   ; [| Field.of_string
          "42536477740858058164730818130587261149155820207748153094480456895727052896150"
      ; Field.of_string
          "45297707210835305388426482743535401273114010430724989418303851665124351001731"
      ; Field.of_string
          "28263543670875633354854018109712021307749750769690268127459707194207091046997"
     |]
   ; [| Field.of_string
          "40809484989590048522440442751358616303471639779690405026946053699354967624695"
      ; Field.of_string
          "51589519265418587649124543325590658874910911006853535317847189422703251228717"
      ; Field.of_string
          "73459936981642894525955700397592343967482441686326322443228255968694436816673"
     |]
   ; [| Field.of_string
          "87298777232393189731949522229743081866971743270330772607820990832164835738703"
      ; Field.of_string
          "23328534428894097247289332213412175849711532153957647506361455182140450133738"
      ; Field.of_string
          "51807348624578081645565456865744011145427112815128832643950401419083788780028"
     |]
   ; [| Field.of_string
          "62003629107726929116302469001779155132709624140360743951550189738290955064278"
      ; Field.of_string
          "109311858027068383034683875948676795998030610067675200794951297783857157095297"
      ; Field.of_string
          "2085588517087605436136379278738013214233743532079287631079316773925068862732"
     |]
   ; [| Field.of_string
          "9513664655545306376987968929852776467090105742275395185801917554996684570014"
      ; Field.of_string
          "91103467624252027317764670613760419385374004736848754250298970998535616755199"
      ; Field.of_string
          "39500000352127197728032684892425352332461947514533659433380855624868454474623"
     |]
   ; [| Field.of_string
          "75175260486328125629270378861920310368403601365269629778076078053196928460032"
      ; Field.of_string
          "56923881233337629517433981230592855430598464522180216309153828833928801967999"
      ; Field.of_string
          "20981004218820236011689230170078809973840534961691702543937445515733151438851"
     |]
   ; [| Field.of_string
          "73175203586574092105626230272409823792532423094740797516874387144340145138310"
      ; Field.of_string
          "45186992623753580336479418079070607289916086076906975839720879934817804495460"
      ; Field.of_string
          "96084125187548549854900995260973117424750860440064269432639526863495781270780"
     |]
   ; [| Field.of_string
          "53530507055579550362119832302266967544350117012822630711681736383163390079758"
      ; Field.of_string
          "24484677147631687826970700541691541659768738376645174313438582486313045584324"
      ; Field.of_string
          "99915577684197600584703320523786830947563355229812244982453188909016758004559"
     |]
   ; [| Field.of_string
          "73101441225016284181831039876112223954723401962484828024235461623078642642543"
      ; Field.of_string
          "57434882751817972247799186935032874577110609253567900895922769490031350316077"
      ; Field.of_string
          "73837027842771758252813592393497967898989365991569964687267097531033696791279"
     |] |]

[%%else]

[%%show
curve_size]

[%%error
"invalid value for \"curve_size\""]

[%%endif]
