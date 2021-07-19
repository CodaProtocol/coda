<?php 

$host = "localhost"; 
$user = "postgres"; 
$pass = "abc123"; 
$db = "minanet"; 

$con = pg_connect("host=$host dbname=$db user=$user password=$pass")
    or die ("Could not connect to server\n"); 

$query = "SELECT * FROM node_record_table LIMIT 5"; 

$rs = pg_query($con, $query) or die("Cannot execute query: $query\n");

while ($row = pg_fetch_row($rs)) {
  echo "$row[0] $row[1] $row[2]\n";
}

pg_close($con); 

?>