import { Badge, IconButton, Tooltip } from '@mui/material';
import ShoppingCartOutlinedIcon from '@mui/icons-material/ShoppingCartOutlined';
import { useTranslation } from 'react-i18next';
import { Link as RouterLink, useLocation } from 'react-router-dom';
import {
  MARKETPLACE_CART_PATH,
  requestOpenMarketplaceCart,
  useMarketplaceCartSummary,
} from '../features/marketplace/cartSummary';

const MARKETPLACE_PATH = '/marketplace';

const buttonSx = {
  minWidth: 44,
  minHeight: 44,
  color: 'text.primary',
  // Focus ring comes from the theme's global :focus-visible outline.
} as const;

/**
 * Header cart control. Always visible on marketplace routes (even with an
 * empty cart) and on any other public page while the stored cart has items,
 * so a returning visitor can find what they saved.
 */
export default function MarketplaceCartButton() {
  const { t } = useTranslation();
  const location = useLocation();
  const { count } = useMarketplaceCartSummary();
  const onMarketplace = location.pathname === MARKETPLACE_PATH;
  const onMarketplaceRoute = onMarketplace || location.pathname.startsWith(`${MARKETPLACE_PATH}/`);
  if (!onMarketplaceRoute && count <= 0) return null;

  const label = count <= 0
    ? t('authEntry.cartEmpty')
    : count === 1
      ? t('authEntry.cartOne')
      : t('authEntry.cartMany', { count });
  const icon = (
    <Badge
      badgeContent={count}
      color="primary"
      invisible={count <= 0}
      max={99}
      data-testid="marketplace-cart-badge"
    >
      <ShoppingCartOutlinedIcon />
    </Badge>
  );

  return (
    <Tooltip title={label}>
      {onMarketplace ? (
        <IconButton
          aria-label={label}
          aria-haspopup="dialog"
          data-testid="marketplace-cart-button"
          onClick={() => requestOpenMarketplaceCart()}
          sx={buttonSx}
        >
          {icon}
        </IconButton>
      ) : (
        <IconButton
          component={RouterLink}
          to={MARKETPLACE_CART_PATH}
          aria-label={label}
          data-testid="marketplace-cart-button"
          sx={buttonSx}
        >
          {icon}
        </IconButton>
      )}
    </Tooltip>
  );
}
