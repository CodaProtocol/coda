

<?php
// $host = "localhost";
// $username = "postgres";
// $password = "abc123";
// $database_name = "minanet";
// $port = "5432";
// $conn = pg_connect("host=".$host." port=".$port." dbname=".$database_name." user=".$username." password=".$password."") or die("Connection failed: " .pg_last_error());
// return $conn
?>

<?php

$username = "minanetuser";
$password = "minanetuser";
$database_name = "minanetdb";
$port = "5432";
$host = "172.31.21.48";
$conn = pg_connect("host=".$host." port=".$port." dbname=".$database_name." user=".$username." password=".$password."") or die("Connection failed: " .pg_last_error());

return $conn

?>