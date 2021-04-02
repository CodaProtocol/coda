<?php
require_once ("connection.php");

if (! (isset($_GET['pageNumber']))) {
    $pageNumber = 1;
} else {
    $pageNumber = $_GET['pageNumber'];
}

$perPageCount = 20;

$sql = "SELECT block_producer_key , score FROM node_record_table ORDER BY score DESC";


if ($result = pg_query($conn, $sql)) {
    $rowCount = pg_num_rows($result);
    pg_free_result($result);
}

$pagesCount = ceil($rowCount / $perPageCount);

$lowerLimit = ($pageNumber - 1) * $perPageCount;

$sqlQuery = "SELECT block_producer_key , score FROM node_record_table ORDER BY score DESC OFFSET ". ($lowerLimit) . " LIMIT " . ($perPageCount);

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
                        <td><?php echo $data['score'] ?></td>
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
      <a class="page-link" href="avascript:void(0);" tabindex="-1" onclick="showRecords('<?php echo $perPageCount;  ?>', '<?php  echo 1;  ?>');">First</a>
    </li>
    <li class="<?php if($pageNumber <= 1) {echo 'page-item disabled';} else {echo 'page-item';}?>">
        <a class="page-link" href="avascript:void(0);" onclick="showRecords('<?php echo $perPageCount;  ?>', '<?php if($pageNumber <= 1){ echo $pageNumber; } else { echo ($pageNumber - 1); } ?>');">Prev</a></li>
    <li class="<?php if($pageNumber == $pagesCount) {echo 'page-item disabled';} else {echo 'page-item';}?>">
        <a class="page-link" href="avascript:void(0);" onclick="showRecords('<?php echo $perPageCount;  ?>', '<?php if($pageNumber >= $pagesCount){ echo $pageNumber; } else { echo ($pageNumber + 1); } ?>');">Next</a></li>
    <li class="<?php if($pageNumber == $pagesCount) {echo 'page-item disabled';} else {echo 'page-item';}?>">
      <a class="page-link" href="avascript:void(0);" onclick="showRecords('<?php echo $perPageCount;  ?>', '<?php  echo $pagesCount;  ?>');">Last</a>
    </li>
  </ul>
</nav>



<table width="50%" align="center">
    <tr>

        <td valign="top" align="left"></td>


        <td valign="top" align="center" id = "pagination">
 
	<?php
	for ($i = 1; $i <= $pagesCount; $i ++) {
    if ($i == $pageNumber) {
        ?>
	      <a href="javascript:void(0);" class="current"><?php echo $i ?></a>
<?php
    } else {
        ?>
	      <a href="javascript:void(0);" class="pages"
            onclick="showRecords('<?php echo $perPageCount;  ?>', '<?php echo $i; ?>');"><?php echo $i ?></a>
<?php
    } // endIf
} // endFor

?>
</td>
        <td align="right" valign="top">
	     Page <?php echo $pageNumber; ?> of <?php echo $pagesCount; ?>
	</td>
    </tr>
</table>