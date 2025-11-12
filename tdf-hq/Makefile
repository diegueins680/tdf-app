.PHONY: up down logs ps restart seed health clean

up:
	@docker compose up -d --build

down:
	@docker compose down

logs:
	@docker compose logs -f app db

ps:
	@docker compose ps

restart:
	@docker compose restart app

seed:
	@curl -s -X POST http://localhost:8080/admin/seed -H "Authorization: Bearer admin-token" -w "\n%{http_code}\n"

health:
	@curl -s http://localhost:8080/health | jq . || curl -s http://localhost:8080/health

version:
	@curl -fsS "$(APP_BASE_URL)/meta/version" | jq .

clean:
	@docker compose down -v
