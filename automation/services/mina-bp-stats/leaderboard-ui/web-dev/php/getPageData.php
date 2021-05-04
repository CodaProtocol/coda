<?php
require_once ("connection.php");
$myarray = include 'config.php';

$ShowScoreColumn = $myarray[0];

if (! (isset($_GET['pageNumber']))) {
    $pageNumber = 1;
} else {
    $pageNumber = $_GET['pageNumber'];
}

$perPageCount = 120;

$sql = "SELECT COUNT(*) FROM node_record_table WHERE application_status = true";


if ($result = pg_query($conn, $sql)) {
    $row = pg_fetch_row($result);
    $rowCount = $row[0];
    pg_free_result($result);
}

$pagesCount = ceil($rowCount / $perPageCount);

$lowerLimit = ($pageNumber - 1) * $perPageCount;


$sqlQuery = "SELECT block_producer_key , score ,score_percent FROM node_record_table WHERE application_status = true ORDER BY score DESC OFFSET ". ($lowerLimit) . " LIMIT " . ($perPageCount);

$results = pg_query($conn, $sqlQuery);
$row = pg_fetch_all($results);

?>

<div class="container pr-0 pl-0 mt-0 mb-5" id="results">
        <div class="table-responsive table-responsive-sm table-responsive-md table-responsive-lg table-responsive-xl">
            <table class="table table-striped text-center w-auto">
                <thead>
                    <tr class="border-top-0">
                        <th scope="col">RANK</th>
                        <th scope="col" class="text-left">PUBLIC KEY</th>
                        <?php 
                        if($ShowScoreColumn == true){
                        ?>
                        <th scope="col">SCORE</th>
                        <?php }?>
                        <th scope="col">60 Day Uptime Performance SCORE</th>
                    </tr>
                </thead>
                <tbody class="">
                <?php 
                 $counter = $lowerLimit + 1;
                foreach ($row as $key => $data) { 
                   
                    ?>
                    <tr>
                        <td scope="row"><?php echo $counter ?></td>
                        <td><?php echo $data['block_producer_key'] ?></td>
                        <?php 
                        if($ShowScoreColumn == true){
                        ?>
                        <td><?php echo $data['score'] ?></td>
                        <?php }?>
                        <td><?php echo $data['score_percent'] ?> %</td>
                    </tr>
                    <?php
                     $counter++;
    }
    ?>
                </tbody>
            </table>
        </div>
    </div>

<div style="height: 30px;"></div>



<nav aria-label="Page navigation example">
  <ul class="pagination justify-content-center">
    <li class="<?php if($pageNumber <= 1) {echo 'page-item disabled';} else {echo 'page-item';}?>">
      <a class="page-link" href="javascript:void(0);" tabindex="-1" onclick="showRecords('<?php echo $perPageCount;  ?>', '<?php  echo 1;  ?>');">First</a>
    </li>
    <li class="<?php if($pageNumber <= 1) {echo 'page-item disabled';} else {echo 'page-item';}?>">
        <a class="page-link" href="javascript:void(0);" onclick="showRecords('<?php echo $perPageCount;  ?>', '<?php if($pageNumber <= 1){ echo $pageNumber; } else { echo ($pageNumber - 1); } ?>');">Prev</a></li>
    <li class="<?php if($pageNumber == $pagesCount) {echo 'page-item disabled';} else {echo 'page-item';}?>">
        <a class="page-link" href="javascript:void(0);" onclick="showRecords('<?php echo $perPageCount;  ?>', '<?php if($pageNumber >= $pagesCount){ echo $pageNumber; } else { echo ($pageNumber + 1); } ?>');">Next</a></li>
    <li class="<?php if($pageNumber == $pagesCount) {echo 'page-item disabled';} else {echo 'page-item';}?>">
      <a class="page-link" href="javascript:void(0);" onclick="showRecords('<?php echo $perPageCount;  ?>', '<?php  echo $pagesCount;  ?>');">Last</a>
    </li>
    <li class = "ml-5 p-2">Page <?php echo $pageNumber; ?> of <?php echo $pagesCount; ?></li>
  </ul>
</nav>
