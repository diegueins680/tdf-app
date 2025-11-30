.PHONY: up down logs ps restart seed health clean export-data init-fresh-db schema-docs

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

# Export current database data to SQL file
export-data:
	@./scripts/export_data.sh

# Initialize fresh database with schema (optionally pass data file as DATA_FILE=path/to/file.sql)
init-fresh-db:
	@./scripts/init_fresh_db.sh $(DATA_FILE)

# Generate schema documentation
schema-docs:
	@echo "Schema documentation: sql/init_schema.sql"
	@echo "Total tables: $$(grep -c 'CREATE TABLE' sql/init_schema.sql)"
	@echo ""
	@echo "Core tables:"
	@grep 'CREATE TABLE' sql/init_schema.sql | grep -v UUID | head -10
