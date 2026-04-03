<?php
require_once("db_config.php");

$input = file_get_contents("php://input");
error_log("RAW INPUT: " . $input); // add this for debugging
$data = json_decode($input, true);
error_log("DECODED: " . print_r($data, true)); // and this

$worker     = $data['worker']     ?? '';
$assignment = $data['assignment'] ?? '';
$total      = $data['total']      ?? 0;
$subject    = $data['subject']    ?? '';
$events     = $data['events']     ?? '';
$actions    = $data['actions']    ?? '';
$notes      = $data['notes']      ?? '';

$stmt = $pdo->prepare("
  INSERT INTO message_pilot_1 (worker, assignment, total, subject, events, actions, notes)
  VALUES (:worker, :assignment, :total, :subject, :events, :actions, :notes)
");

$stmt->execute([
  ':worker'     => $worker,
  ':assignment' => $assignment,
  ':total'      => $total,
  ':subject'    => $subject,
  ':events'     => $events,
  ':actions'    => $actions,
  ':notes'      => $notes,
]);

echo json_encode(['status'=>'success']);

?>