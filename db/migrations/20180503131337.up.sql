CREATE TABLE "users" (
    "id" INTEGER PRIMARY KEY AUTOINCREMENT,
    "github_id" STRING NOT NULL,
    "slack_id" STRING NOT NULL,
    "created_at" TIMESTAMP,
    "updated_at" TIMESTAMP
);
