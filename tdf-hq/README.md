# TDF Records - Backend API

This directory contains the Haskell backend for the TDF Records Platform. It provides a robust, type-safe API that serves as the backbone for the web and mobile applications.

## Architecture & Tech Stack

-   **Language**: Haskell
-   **API Framework**: [Servant](https://docs.servant.dev/) for creating a type-safe REST API.
-   **Database**: PostgreSQL
-   **ORM**: [Persistent](http://www.yesodweb.com/book/persistent) for database interactions.
-   **Build Tool**: [Stack](https://docs.haskellstack.org/en/stable/)
-   **Containerization**: Docker and Docker Compose for development and production environments.

## Key Features

-   **Type-Safe API**: The API, defined in `TDF.API`, ensures that handlers, routes, and clients are always in sync.
-   **Automatic OpenAPI Generation**: The backend automatically generates an OpenAPI specification (`docs/openapi/lessons-and-receipts.yaml`), which is used to create TypeScript clients for the frontend apps.
-   **Containerized Development**: Full Docker support for running the API and its PostgreSQL database, ensuring a consistent and isolated development environment.
-   **Database Seeding**: A `make seed` command is available to populate the development database with realistic sample data.

## Getting Started

### Prerequisites

-   [Haskell Stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/)
-   [Docker](https://www.docker.com/products/docker-desktop/) and Docker Compose

### Development Setup

1.  **Navigate to the Backend Directory**:
    ```bash
    cd tdf-hq
    ```

2.  **Configure Environment**:
    Copy the default environment configuration to a new `.env` file. This file stores secrets and environment-specific settings.
    ```bash
    cp config/default.env .env
    ```
    *   When running locally with `stack`, ensure `DB_HOST` is set to `127.0.0.1` or `localhost`.
    *   When using Docker, the `docker-compose.yml` file sets `DB_HOST` to `db`, which is the service name of the PostgreSQL container.

3.  **Launch Services**:
    Use the provided Makefile to build and start the API and database containers.
    ```bash
    make up
    ```
    This command will start a PostgreSQL container and the Haskell API container. The API server will be accessible at `http://localhost:8080`.

4.  **Seed the Database (Optional)**:
    To populate the database with initial development data, run:
    ```bash
    make seed
    ```

## Development Workflow

### Running the Application
-   **With Docker (Recommended)**:
    -   `make up`: Start all services.
    -   `make down`: Stop all services.
    -   `make logs`: View logs from the running containers.
    -   `make restart`: Restart the services.
    -   `make health`: Check the health status of the database and API.
-   **Locally with Stack**:
    -   Run `stack run` to build and start the server.
    -   Use `stack ghci` for an interactive REPL, which is excellent for testing modules and functions interactively.

### API Development Pattern
1.  **Define Types**: Start by updating the API types in `TDF.API`.
2.  **Implement Handlers**: Add or modify the corresponding server logic in `TDF.Server`.
3.  **Add DTOs**: Create or update Data Transfer Objects in `TDF.DTO`.
4.  **Database Logic**: Implement the necessary database queries and models in `TDF.DB` and `TDF.Models`.
5.  **Generate Clients**: After making changes to the API, navigate to the project root and run `npm run generate:api:ui` and `npm run generate:api:mobile` to update the frontend clients.
