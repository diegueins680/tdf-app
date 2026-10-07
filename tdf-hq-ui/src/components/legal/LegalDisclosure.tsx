import { useId, useState, type ReactNode } from 'react';
import { Box, ButtonBase, Collapse, Stack, Typography, useMediaQuery } from '@mui/material';
import ExpandMoreIcon from '@mui/icons-material/ExpandMore';

export type LegalDisclosureLanguage = 'es' | 'en';

const actionCopy: Record<LegalDisclosureLanguage, { expand: string; collapse: string }> = {
  es: { expand: 'Leer completo', collapse: 'Ocultar' },
  en: { expand: 'Read in full', collapse: 'Hide' },
};

export interface LegalDisclosureProps {
  /** Document name shown in the header row, e.g. "Política de reembolso". */
  title: ReactNode;
  /** The full legal text. It stays in the DOM while collapsed so it is never lost, only hidden. */
  children: ReactNode;
  /** Short context visible while collapsed, e.g. the terms version. */
  summary?: ReactNode;
  language?: LegalDisclosureLanguage;
  /** Wraps the header button in a heading so screen-reader heading navigation finds it. */
  headingLevel?: 2 | 3 | 4 | 5 | 6;
  /** Legal text is collapsed by default; only open it initially when the user deep-linked to it. */
  defaultExpanded?: boolean;
  dense?: boolean;
  id?: string;
}

/**
 * Disclosure (WAI-ARIA APG pattern) for terms, policies, consent texts and disclaimers.
 * Expanded state is component state, so it survives re-renders on the same page or step and
 * resets on navigation; it is deliberately never persisted.
 * Consent controls must stay outside this component so they remain visible while collapsed.
 */
export function LegalDisclosure({
  title,
  children,
  summary,
  language = 'es',
  headingLevel,
  defaultExpanded = false,
  dense = false,
  id,
}: LegalDisclosureProps) {
  const generatedId = useId();
  const baseId = id ?? `legal-disclosure-${generatedId.replace(/:/g, '')}`;
  const buttonId = `${baseId}-button`;
  const panelId = `${baseId}-panel`;
  const [expanded, setExpanded] = useState(defaultExpanded);
  const reduceMotion = useMediaQuery('(prefers-reduced-motion: reduce)', { noSsr: true });
  const action = actionCopy[language];

  const header = (
    <ButtonBase
      id={buttonId}
      aria-expanded={expanded}
      aria-controls={panelId}
      onClick={() => setExpanded((current) => !current)}
      disableRipple
      sx={{
        width: '100%',
        minHeight: dense ? 44 : 48,
        px: dense ? 1.5 : 2,
        py: 1,
        textAlign: 'left',
        borderRadius: 'inherit',
        transition: 'background-color 150ms ease',
        '&:hover': { bgcolor: 'action.hover' },
        '&.Mui-focusVisible': { outline: '3px solid', outlineColor: 'primary.main', outlineOffset: -3 },
      }}
    >
      <Box
        sx={{
          display: 'grid',
          width: '100%',
          alignItems: 'center',
          columnGap: 1.5,
          rowGap: 0.25,
          // On phones the action label drops under the title so long document names keep the width.
          gridTemplateColumns: { xs: 'minmax(0, 1fr) auto', sm: 'minmax(0, 1fr) auto auto' },
          gridTemplateAreas: { xs: '"text icon" "action icon"', sm: '"text action icon"' },
        }}
      >
        <Stack spacing={0.25} sx={{ gridArea: 'text', minWidth: 0 }}>
          <Typography component="span" variant={dense ? 'body2' : 'subtitle2'} fontWeight={700} sx={{ overflowWrap: 'anywhere' }}>
            {title}
          </Typography>
          {summary && (
            <Typography component="span" variant="body2" color="text.secondary" sx={{ fontSize: '0.8125rem', overflowWrap: 'anywhere' }}>
              {summary}
            </Typography>
          )}
        </Stack>
        <Typography component="span" variant="body2" fontWeight={600} color="primary" sx={{ gridArea: 'action' }}>
          {expanded ? action.collapse : action.expand}
        </Typography>
        <ExpandMoreIcon
          aria-hidden
          color="primary"
          sx={{
            gridArea: 'icon',
            transform: expanded ? 'rotate(180deg)' : 'none',
            transition: reduceMotion ? 'none' : 'transform 150ms ease',
          }}
        />
      </Box>
    </ButtonBase>
  );

  return (
    <Box
      data-testid="legal-disclosure"
      sx={{ border: 1, borderColor: 'divider', borderRadius: 2, bgcolor: 'background.paper', minWidth: 0 }}
    >
      {headingLevel ? (
        <Typography component={`h${headingLevel}`} variant="inherit" sx={{ m: 0, font: 'inherit' }}>
          {header}
        </Typography>
      ) : (
        header
      )}
      <Collapse in={expanded} timeout={reduceMotion ? 0 : 'auto'}>
        <Box
          id={panelId}
          role="region"
          aria-labelledby={buttonId}
          sx={{
            px: dense ? 1.5 : 2,
            pb: dense ? 1.5 : 2,
            pt: 0.5,
            overflowWrap: 'anywhere',
            '& > * + *': { mt: 1 },
          }}
        >
          {children}
        </Box>
      </Collapse>
    </Box>
  );
}

export default LegalDisclosure;
