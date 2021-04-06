<?php
include("connection.php");
class Blocks {
	protected $conn;
	protected $data = array();
	function __construct() {

		$db = new dbObj();
		$connString =  $db->getConnstring();
		$this->conn = $connString;
	}
	
    
	public function getBlockesData() {
		$sql = "SELECT * FROM node_record_table LIMIT 20";
		$queryRecords = pg_query($this->conn, $sql) or die("error to fetch Blocks data");
		$data = pg_fetch_all($queryRecords);
		return $data;
	}
}
?>