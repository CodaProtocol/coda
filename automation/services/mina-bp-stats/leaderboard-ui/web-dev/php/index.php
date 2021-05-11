<!DOCTYPE html>
<html lang="en">

<head>
    <meta charset="UTF-8">
    <meta http-equiv="X-UA-Compatible" content="IE=edge">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Uptime Leaderboard</title>
    
    <link rel="stylesheet" href="https://stackpath.bootstrapcdn.com/bootstrap/4.4.1/css/bootstrap.min.css" integrity="sha384-Vkoo8x4CGsO3+Hhxv8T/Q5PaXtkKtu6ug5TOeNV6gBiFeWPGFN9MuhOf23Q9Ifjh" crossorigin="anonymous">
    <script src="https://code.jquery.com/jquery-3.4.1.slim.min.js" integrity="sha384-J6qa4849blE2+poT4WnyKhv5vZF5SrPo0iEjwBvKU7imGFAV0wwj1yYfoRSJoZ+n" crossorigin="anonymous"></script>
    <script src="https://cdn.jsdelivr.net/npm/popper.js@1.16.0/dist/umd/popper.min.js" integrity="sha384-Q6E9RHvbIyZFJoft+2mJbHaEWldlvI9IOYy5n3zV9zzTtmI3UksdQRVvoxMfooAo" crossorigin="anonymous"></script>
    <script src="https://stackpath.bootstrapcdn.com/bootstrap/4.4.1/js/bootstrap.min.js" integrity="sha384-wfSDF2E50Y2D1uUdj0O3uMBJnjuUD4Ih7YwaYd1iqfktj0Uod8GCExl3Og8ifwB6" crossorigin="anonymous"></script>

    <link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/bootstrap-icons@1.4.0/font/bootstrap-icons.css">
    
    <link rel="stylesheet" href="assets/css/custome.css">
    <link rel="stylesheet" href="assets/css/responsive.css">
    
    <script src="https://ajax.googleapis.com/ajax/libs/jquery/2.1.3/jquery.min.js"></script>
</head>

<body>
    <div class="container">

        <!-- Logo And Header Section Start -->
        <div class="row mb-3">
            <img src="assets/images/MinaWordmark.png" alt="Mina" class="mina-main-logo">
        </div>
        <div class="row mb-5">
            <div class="subheader">
                <p class="mina-subheader-text-font">Block Producers Uptime Tracker </p>
            </div>
        </div>
        <!-- Logo And Header Section End -->

        <!-- Top Button and Link Section Start -->
        <div class="row mb-5">
            <div class="uptime-lederboard-topButton"></div>
            <div class="col-12 col-md-6 mx-0 px-0 topButton">
                <button type="button" class="delegationButton btn btn-dark btn-primary" onclick="window.open('https://docs.google.com/forms/d/e/1FAIpQLSduM5EIpwZtf5ohkVepKzs3q0v0--FDEaDfbP2VD4V6GcBepA/viewform')">APPLY FOR DELEGATION <i class="bi bi-arrow-right "></i>
                </button>
                <div class="bottomPlate for-normal" id="leaderBoardbtn">
                </div>
            </div>
            <div class="col-12 col-md-6  Link-responcive">
                <div class="row d-flex mb-2">
                    <a class="Mina-Refrance-color ml-auto alignment-link" href="https://forums.minaprotocol.com/t/delegation-program-faq/4246" target="_blank">FAQ</a><i class="ml-2 bi bi-box-arrow-up-right Mina-Refrance-color"></i>
                </div>
                <div class="row Link-responcive">
                    <a class="Mina-Refrance-color ml-auto alignment-link" href="https://minaprotocol.com/blog/mina-foundation-delegation-policy" target="_blank">Mina Foundation Delegation Policy</a><i class="ml-2 bi bi-box-arrow-up-right Mina-Refrance-color"></i>
                    <!-- <a class="Mina-Refrance-color ml-auto alignment-link" href="https://medium.com/o1labs/o-1-labs-delegation-policy-786bf96f9fdd" target="_blank">O(1) Labs Delegation Policy</a><i class="ml-2 bi bi-box-arrow-up-right Mina-Refrance-color"></i> -->
                </div>
            </div>
        </div>
        <!-- Top Button and Link Section End -->
    </div>



    <!-- Data Table Section Start -->
    <div id="results"></div>
    <div id="loader"></div>

    
    <!-- Data Table Section End -->

    

    <script type="text/javascript">
    function showRecords(perPageCount, pageNumber) {
        $.ajax({
            type: "GET",
            url: "getPageData.php",
            data: "pageNumber=" + pageNumber,
            cache: false,
    		beforeSend: function() {
                $('#loader').html('<div class="spinner-border d-flex mx-auto" role="status"><span class="sr-only">Loading...</span></div>');
    			
            },
            success: function(html) {
                $("#results").html(html);
                $('#loader').html(''); 
            }
        });
    }
    
    $(document).ready(function() {
        showRecords(10, 1);
    });
</script>
</body>

</html>