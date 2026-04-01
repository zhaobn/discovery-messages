<?php
require_once("db_config.php");

$input = file_get_contents("php://input");
$data = json_decode($input, true);

$worker = $data['worker'] ?? '';
$assignment = $data['assignment'] ?? '';
$total = $data['total'] ?? 0;
$subject = $data['subject'] ?? '';
$events = $data['events'] ?? '';
$actions = $data['actions'] ?? '';

$stmt = $pdo->prepare("
    INSERT INTO grid_pilot_1 (worker, assignment, total, subject, events, actions)
    VALUES (:worker, :assignment, :total, :subject, :events, :actions)
");
$stmt->execute([
    ':worker' => $worker,
    ':assignment' => $assignment,
    ':total' => $total,
    ':subject' => $subject,
    ':events' => $events,
    ':actions' => $actions
]);

echo json_encode(['status'=>'success']);

?>