#!/bin/bash
# Stripe Ticketing System Deployment Script for TDF-Label
# Usage: ./deploy-stripe-ticketing.sh [environment]
# Environments: local, staging, production

set -e

ENV=${1:-local}
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(dirname "$SCRIPT_DIR")"

echo "🚀 TDF Stripe Ticketing Deployment"
echo "==================================="
echo "Environment: $ENV"
echo ""

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

log_info() {
    echo -e "${GREEN}[INFO]${NC} $1"
}

log_warn() {
    echo -e "${YELLOW}[WARN]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Check prerequisites
check_prerequisites() {
    log_info "Checking prerequisites..."
    
    if ! command -v stack &> /dev/null; then
        log_error "Stack not found. Install Haskell Stack first."
        exit 1
    fi
    
    if [ "$ENV" != "local" ]; then
        if ! command -v flyctl &> /dev/null; then
            log_error "flyctl not found. Install Fly.io CLI first."
            exit 1
        fi
    fi
    
    log_info "Prerequisites OK"
}

# Build backend
build_backend() {
    log_info "Building backend..."
    cd "$PROJECT_DIR/tdf-hq"
    
    if [ "$ENV" = "local" ]; then
        stack build --fast
    else
        stack build
    fi
    
    log_info "Backend build successful"
}

# Deploy backend to Fly.io
deploy_backend_fly() {
    log_info "Deploying backend to Fly.io..."
    cd "$PROJECT_DIR"
    
    # Check if required secrets are set
    if ! flyctl secrets list --app tdf-hq | grep -q "STRIPE_SECRET_KEY"; then
        log_warn "STRIPE_SECRET_KEY not set in Fly.io secrets!"
        log_warn "Set it with: flyctl secrets set STRIPE_SECRET_KEY=sk_..."
    fi
    
    if ! flyctl secrets list --app tdf-hq | grep -q "STRIPE_WEBHOOK_SECRET"; then
        log_warn "STRIPE_WEBHOOK_SECRET not set in Fly.io secrets!"
        log_warn "Set it after configuring webhook in Stripe Dashboard"
    fi
    
    flyctl deploy --app tdf-hq
    log_info "Backend deployed to Fly.io"
}

# Deploy frontend to Cloudflare Pages
deploy_frontend_cf() {
    log_info "Deploying frontend to Cloudflare Pages..."
    cd "$PROJECT_DIR/tdf-hq-ui"
    
    # Check if required env vars are set
    if [ -z "$VITE_STRIPE_PUBLISHABLE_KEY" ]; then
        log_warn "VITE_STRIPE_PUBLISHABLE_KEY not set!"
        log_warn "Set it in Cloudflare Pages environment variables"
    fi
    
    npm run build
    
    # Deploy using wrangler or git push
    if command -v wrangler &> /dev/null; then
        wrangler pages deploy dist --project-name=tdf-app
    else
        log_warn "wrangler not found. Push to git for auto-deployment."
    fi
    
    log_info "Frontend deployed"
}

# Run database migrations
run_migrations() {
    log_info "Running database migrations..."
    cd "$PROJECT_DIR/tdf-hq"
    
    if [ "$ENV" = "local" ]; then
        # Local development - apply migration directly
        if [ -f "sql/2026-05-24_ticketing_system_enhancements.sql" ]; then
            log_info "Applying ticketing system migration..."
            # This would need proper DB credentials
            # psql -U postgres -d tdf_hq < sql/2026-05-24_ticketing_system_enhancements.sql
            log_warn "Manual migration required for local DB"
        fi
    else
        log_info "Migrations run automatically on Fly.io deploy (RUN_MIGRATIONS=true)"
    fi
}

# Show post-deployment checklist
show_checklist() {
    echo ""
    echo "==================================="
    echo "📋 Post-Deployment Checklist"
    echo "==================================="
    echo ""
    
    if [ "$ENV" = "local" ]; then
        echo "1. ✅ Backend built successfully"
        echo "2. ⬜ Start backend: cd tdf-hq && stack exec tdf-hq-exe"
        echo "3. ⬜ Start frontend: cd tdf-hq-ui && npm run dev"
        echo "4. ⬜ Configure Stripe CLI for webhook testing:"
        echo "     stripe listen --forward-to localhost:8080/social-events/stripe/webhook"
        echo "5. ⬜ Run end-to-end tests"
    else
        echo "1. ✅ Backend deployed"
        echo "2. ✅ Frontend deployed"
        echo "3. ⬜ Configure Stripe webhook endpoint:"
        echo "     https://tdf-hq.fly.dev/social-events/stripe/webhook"
        echo "4. ⬜ Set STRIPE_WEBHOOK_SECRET in Fly.io secrets"
        echo "5. ⬜ Test payment flow with test card 4242 4242 4242 4242"
        echo "6. ⬜ Verify ticket generation and QR codes"
    fi
    
    echo ""
    echo "Stripe Configuration:"
    echo "  - Webhook endpoint: /social-events/stripe/webhook"
    echo "  - Required events: payment_intent.succeeded, payment_intent.payment_failed"
    echo "  - Test card: 4242 4242 4242 4242"
    echo ""
}

# Main deployment flow
main() {
    check_prerequisites
    
    case "$ENV" in
        local)
            build_backend
            run_migrations
            ;;
        staging|production)
            build_backend
            run_migrations
            deploy_backend_fly
            deploy_frontend_cf
            ;;
        *)
            log_error "Unknown environment: $ENV"
            echo "Usage: $0 [local|staging|production]"
            exit 1
            ;;
    esac
    
    show_checklist
    
    log_info "Deployment process complete!"
}

main "$@"
