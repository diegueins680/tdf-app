import { createCdpClient } from './openclaw-cdp.mjs';

const SRI_INVOICE_URL =
  'https://facturadorsri.sri.gob.ec/portal-facturadorsri-internet/pages/comprobantes/factura/Factura.html';
const SRI_PRODUCTS_URL =
  'https://facturadorsri.sri.gob.ec/portal-facturadorsri-internet/pages/catalogos/administracion/productos.html';
const SRI_START_URL =
  'https://facturadorsri.sri.gob.ec/portal-facturadorsri-internet/pages/inicio.html';

function sanitizeProductCode(input) {
  return input
    .normalize('NFD')
    .replace(/[\u0300-\u036f]/g, '')
    .toUpperCase()
    .replace(/[^A-Z0-9]/g, '')
    .slice(0, 25);
}

export function mapTaxBpsToSriIvaCode(taxBps) {
  if (taxBps == null) return '0';
  switch (Number(taxBps)) {
    case 0:
      return '0';
    case 500:
      return '5';
    case 1500:
      return '4';
    default:
      throw new Error(`Unsupported SRI tax rate for ${taxBps} bps. Use sriIvaCode explicitly.`);
  }
}

export function centsToDecimal(cents) {
  return Number((Number(cents) / 100).toFixed(2));
}

export function computeInvoiceTotal(lines) {
  return Number(
    lines
      .reduce((sum, line) => sum + centsToDecimal(line.unitCents) * Number(line.quantity ?? 1), 0)
      .toFixed(2),
  );
}

export function normalizeSriLine(line) {
  const quantity = Math.max(1, Number(line.quantity ?? 1));
  const description = String(line.description || '').trim();
  if (!description) {
    throw new Error('SRI line description is required');
  }
  const code = sanitizeProductCode(
    line.code || `${description}${String(line.unitCents ?? '').trim()}`,
  );
  if (!code) {
    throw new Error(`Could not derive a valid SRI product code for "${description}"`);
  }

  return {
    code,
    auxiliaryCode: sanitizeProductCode(line.auxiliaryCode || code),
    description: description.toUpperCase(),
    quantity,
    unitPrice: centsToDecimal(line.unitCents),
    sriIvaCode: String(line.sriIvaCode || mapTaxBpsToSriIvaCode(line.taxBps)),
    additionalInfo: String(line.additionalInfo || line.sriAdditionalInfo || description)
      .trim()
      .toUpperCase(),
  };
}

export function extractIssuedInvoiceData(bodyText) {
  const compact = String(bodyText || '').replace(/\s+/g, ' ').trim();
  const authorizationNumber =
    compact.match(/Número de autorización:\s*([0-9]+)/i)?.[1] || null;
  const invoiceNumber =
    compact.match(/Número de comprobante:\s*([0-9-]+)/i)?.[1] || null;
  const buyerEmail =
    compact.match(/Correo enviado a:\s*([^\s]+)/i)?.[1] || null;
  return {
    authorizationNumber,
    invoiceNumber,
    buyerEmail,
    compactBody: compact,
  };
}

function visibleInputSelector() {
  return `
    (() => {
      const dialog = [...document.querySelectorAll('.ui-dialog, [role=dialog]')]
        .find((node) => getComputedStyle(node).display !== 'none' && /Clave del certificado/i.test(node.innerText || ''));
      if (!dialog) return null;
      const input = [...dialog.querySelectorAll('input')]
        .find((node) => getComputedStyle(node).display !== 'none' && node.offsetParent !== null);
      return input ? { id: input.id || null, type: input.type || null } : null;
    })()
  `;
}

function jsLiteral(value) {
  return JSON.stringify(value);
}

async function waitForText(client, targetId, matcher, timeoutMs = 15_000) {
  const started = Date.now();
  while (Date.now() - started < timeoutMs) {
    const body = await client.evaluate(
      targetId,
      "(() => (document.body.innerText || '').replace(/\\s+/g, ' '))()",
    );
    if (matcher(body)) {
      return body;
    }
    await new Promise((resolve) => setTimeout(resolve, 400));
  }
  throw new Error('Timed out waiting for expected SRI page state');
}

function findTarget(targets, predicate) {
  return targets.find((target) => target.type === 'page' && predicate(target));
}

async function getOrCreateTarget(client, urlMatcher, fallbackUrl) {
  const targets = await client.listTargets();
  const existing = findTarget(targets, (target) => urlMatcher(target.url || ''));
  if (existing) {
    return existing.id;
  }
  const created = await client.createTarget(fallbackUrl);
  return created.targetId;
}

async function ensureProduct(client, targetId, line) {
  await client.navigate(targetId, SRI_PRODUCTS_URL);
  await waitForText(client, targetId, (body) => body.includes('Mis productos y servicios'));

  const exists = await client.evaluate(
    targetId,
    `(() => (document.body.innerText || '').replace(/\\s+/g, ' ').includes(${jsLiteral(line.code)}))()`,
  );
  if (exists) {
    return;
  }

  const result = await client.evaluate(
    targetId,
    [
      '(() => new Promise((resolve) => {',
      '  const wait = (ms) => new Promise((r) => setTimeout(r, ms));',
      '  const setValue = (id, value) => {',
      '    const input = document.getElementById(id);',
      '    if (!input) throw new Error(`Missing ${id}`);',
      '    input.focus();',
      '    input.value = value;',
      '    input.dispatchEvent(new Event("input", { bubbles: true }));',
      '    input.dispatchEvent(new Event("change", { bubbles: true }));',
      '    input.dispatchEvent(new Event("blur", { bubbles: true }));',
      '  };',
      '  const setCheckbox = (id, checked) => {',
      '    const input = document.getElementById(id);',
      '    if (!input) return;',
      '    input.checked = checked;',
      '    input.dispatchEvent(new Event("change", { bubbles: true }));',
      '  };',
      '  const run = async () => {',
      `    setValue("formaCrear:j_idt29", ${jsLiteral(line.code)});`,
      `    setValue("formaCrear:j_idt31", ${jsLiteral(line.auxiliaryCode)});`,
      `    setValue("formaCrear:j_idt33", ${jsLiteral(line.description)});`,
      `    setValue("formaCrear:j_idt35", ${jsLiteral(line.unitPrice.toFixed(2))});`,
      `    setValue("formaCrear:j_idt37", ${jsLiteral(line.additionalInfo)});`,
      '    const iva = window.PrimeFaces?.widgets?.widget_formaCrear_selecttipoIVA;',
      `    if (iva?.selectValue) iva.selectValue(${jsLiteral(line.sriIvaCode)});`,
      '    else {',
      '      const select = document.getElementById("formaCrear:selecttipoIVA_input");',
      `      select.value = ${jsLiteral(line.sriIvaCode)};`,
      '      select.dispatchEvent(new Event("change", { bubbles: true }));',
      '    }',
      '    setCheckbox("formaCrear:j_idt44_input", false);',
      '    setCheckbox("formaCrear:j_idt46_input", false);',
      '    document.getElementById("formaCrear:cmbGuardarNuevoProducto").click();',
      '    await wait(1800);',
      '    const body = (document.body.innerText || "").replace(/\\s+/g, " ");',
      '    resolve(JSON.stringify({ ok: body.includes(' + jsLiteral(line.code) + '), body }));',
      '  };',
      '  run().catch((error) => resolve(JSON.stringify({ ok: false, error: String(error) })));',
      '}))()',
    ].join('\n'),
    { awaitPromise: true },
  );

  const parsed = JSON.parse(result);
  if (!parsed.ok) {
    throw new Error(parsed.error || `Could not create SRI product ${line.code}`);
  }
}

async function openInvoicePage(client, targetId) {
  await client.navigate(targetId, SRI_INVOICE_URL);
  await waitForText(client, targetId, (body) => body.includes('Emisión - Factura'));
}

async function fillInvoiceHeader(client, targetId, request) {
  const result = await client.evaluate(
    targetId,
    [
      '(() => new Promise((resolve) => {',
      '  const wait = (ms) => new Promise((r) => setTimeout(r, ms));',
      '  const run = async () => {',
      '    const est = window.PrimeFaces?.widgets?.widget_form_cabeceraComprobanteDlg_j_idt61;',
      '    const point = window.PrimeFaces?.widgets?.widget_form_identifiacionDelComprobante_selectsecuencial;',
      '    const buyerType = window.PrimeFaces?.widgets?.widget_form_busquedaCompradorComp_cmbTipoIdentificacion;',
      `    est?.selectValue(${jsLiteral(request.establishment)});`,
      '    await wait(600);',
      `    point?.selectValue(${jsLiteral(request.emissionPoint)});`,
      '    await wait(600);',
      '    buyerType?.selectValue("R");',
      '    await wait(400);',
      '    const ruc = document.getElementById("form:busquedaCompradorComp:ruc");',
      `    ruc.value = ${jsLiteral(request.customer.ruc)};`,
      '    ruc.dispatchEvent(new Event("input", { bubbles: true }));',
      '    ruc.dispatchEvent(new Event("change", { bubbles: true }));',
      '    document.getElementById("form:busquedaCompradorComp:botonBuscarComprador").click();',
      '    await wait(2200);',
      '    resolve(JSON.stringify({',
      '      ruc: document.getElementById("form:busquedaCompradorComp:ruc")?.value || "",',
      '      legalName: document.getElementById("form:busquedaCompradorComp:compradorRazonSocial")?.value || "",',
      '      email: document.getElementById("form:busquedaCompradorComp:compradorEmail")?.value || "",',
      '      phone: document.getElementById("form:busquedaCompradorComp:compradorTelefono")?.value || ""',
      '    }));',
      '  };',
      '  run().catch((error) => resolve(JSON.stringify({ error: String(error) })));',
      '}))()',
    ].join('\n'),
    { awaitPromise: true },
  );

  const parsed = JSON.parse(result);
  if (parsed.error) {
    throw new Error(parsed.error);
  }
  if (!parsed.legalName) {
    throw new Error(`SRI did not resolve buyer RUC ${request.customer.ruc}`);
  }
  return parsed;
}

async function addInvoiceLine(client, targetId, line) {
  const result = await client.evaluate(
    targetId,
    [
      '(() => new Promise((resolve) => {',
      '  const wait = (ms) => new Promise((r) => setTimeout(r, ms));',
      '  const run = async () => {',
      '    document.getElementById("form:productoBusquedaComposite:j_idt124").click();',
      '    await wait(700);',
      '    const codeInput = document.getElementById("form:productoBusquedaComposite:txtCodigoPrincipal");',
      `    codeInput.value = ${jsLiteral(line.code)};`,
      '    codeInput.dispatchEvent(new Event("input", { bubbles: true }));',
      '    codeInput.dispatchEvent(new Event("change", { bubbles: true }));',
      '    document.getElementById("form:productoBusquedaComposite:btnBuscaProducto").click();',
      '    await wait(1200);',
      '    const selectButton = document.getElementById("form:productoBusquedaComposite:productosBuscar:0:btnSelect");',
      '    if (!selectButton) {',
      '      const body = (document.body.innerText || "").replace(/\\s+/g, " ");',
      '      resolve(JSON.stringify({ ok: false, body }));',
      '      return;',
      '    }',
      '    selectButton.click();',
      '    await wait(1200);',
      '    const row = document.querySelector("#form\\\\:productosData_data > tr:last-child");',
      '    if (row) {',
      '      const inputs = [...row.querySelectorAll("input")].filter((node) => getComputedStyle(node).display !== "none" && node.offsetParent !== null);',
      '      const qtyInput = inputs.find((node) => /cant/i.test(node.id || "") || /cant/i.test(node.name || "")) || inputs[0];',
      '      if (qtyInput) {',
      `        qtyInput.value = ${jsLiteral(String(line.quantity))};`,
      '        qtyInput.dispatchEvent(new Event("input", { bubbles: true }));',
      '        qtyInput.dispatchEvent(new Event("change", { bubbles: true }));',
      '        qtyInput.dispatchEvent(new Event("blur", { bubbles: true }));',
      '        await wait(500);',
      '      }',
      '    }',
      '    const body = (document.body.innerText || "").replace(/\\s+/g, " ");',
      '    resolve(JSON.stringify({ ok: body.includes(' + jsLiteral(line.code) + '), body }));',
      '  };',
      '  run().catch((error) => resolve(JSON.stringify({ ok: false, error: String(error) })));',
      '}))()',
    ].join('\n'),
    { awaitPromise: true },
  );
  const parsed = JSON.parse(result);
  if (!parsed.ok) {
    throw new Error(parsed.error || `Could not add SRI line ${line.code}`);
  }
}

async function addPayment(client, targetId, request) {
  const shortcutId = (() => {
    switch (request.paymentMode) {
      case 'debit':
        return 'form:formaPagoComposite:cmbNuevoDetalleFormaPagoTarjetaDebito';
      case 'credit':
        return 'form:formaPagoComposite:cmbNuevoDetalleFormaPagoTarjetaCredito';
      case 'cash':
      default:
        return 'form:formaPagoComposite:cmbNuevoDetalleFormaPagoEfectivo';
    }
  })();

  await client.evaluate(
    targetId,
    [
      '(() => new Promise((resolve) => {',
      '  const wait = (ms) => new Promise((r) => setTimeout(r, ms));',
      '  const run = async () => {',
      `    document.getElementById(${jsLiteral(shortcutId)}).click();`,
      '    await wait(1200);',
      '    const body = (document.body.innerText || "").replace(/\\s+/g, " ");',
      '    resolve(body);',
      '  };',
      '  run().catch(() => resolve((document.body.innerText || "").replace(/\\s+/g, " ")));',
      '}))()',
    ].join('\n'),
    { awaitPromise: true },
  );
}

async function finalizeInvoice(client, targetId, request) {
  if (!request.signAndSend) {
    await client.evaluate(
      targetId,
      '(() => { document.getElementById("form:guardarSinFirmarButton")?.click(); return true; })()',
    );
    return {
      status: 'saved',
      authorizationNumber: null,
      invoiceNumber: null,
      buyerEmail: request.customer.email || null,
    };
  }

  await client.evaluate(
    targetId,
    '(() => { document.getElementById("form:firmarButtonSinToken")?.click(); return true; })()',
  );
  await waitForText(client, targetId, (body) => body.includes('Clave del certificado'));

  if (!request.certificatePassword) {
    return {
      status: 'awaiting_signature_password',
      authorizationNumber: null,
      invoiceNumber: null,
      buyerEmail: request.customer.email || null,
    };
  }

  const visibleInput = await client.evaluate(targetId, visibleInputSelector());
  if (!visibleInput?.id) {
    throw new Error('Could not find the certificate password input in the SRI signature dialog');
  }

  const result = await client.evaluate(
    targetId,
    [
      '(() => new Promise((resolve) => {',
      '  const wait = (ms) => new Promise((r) => setTimeout(r, ms));',
      '  const run = async () => {',
      `    const input = document.getElementById(${jsLiteral(visibleInput.id)});`,
      '    input.focus();',
      `    input.value = ${jsLiteral(request.certificatePassword)};`,
      '    input.dispatchEvent(new Event("input", { bubbles: true }));',
      '    input.dispatchEvent(new Event("change", { bubbles: true }));',
      '    const sendButton = document.getElementById("form:appletComposite:btnEnviarSinTokenn")',
      '      || [...document.querySelectorAll("button, input[type=submit], a.ui-button")]',
      '           .find((node) => /Enviar/i.test(node.innerText || node.textContent || node.value || ""));',
      '    if (!sendButton) throw new Error("Missing SRI send button");',
      '    sendButton.click();',
      '    await wait(5000);',
      '    resolve((document.body.innerText || "").replace(/\\s+/g, " "));',
      '  };',
      '  run().catch((error) => resolve(`ERROR: ${String(error)}`));',
      '}))()',
    ].join('\n'),
    { awaitPromise: true },
  );

  if (String(result).startsWith('ERROR:')) {
    throw new Error(String(result).slice('ERROR: '.length));
  }

  const parsed = extractIssuedInvoiceData(result);
  if (!parsed.authorizationNumber || !parsed.invoiceNumber) {
    throw new Error(`SRI did not confirm invoice issuance: ${parsed.compactBody.slice(0, 800)}`);
  }
  return { status: 'issued', ...parsed };
}

function normalizeRequest(input) {
  if (!input?.customer?.ruc) {
    throw new Error('SRI customer.ruc is required');
  }
  if (!Array.isArray(input.lines) || input.lines.length === 0) {
    throw new Error('SRI requires at least one invoice line');
  }

  const lines = input.lines.map(normalizeSriLine);
  return {
    establishment: String(input.establishment || process.env.SRI_ESTABLISHMENT || '1'),
    emissionPoint: String(input.emissionPoint || process.env.SRI_EMISSION_POINT || '100'),
    paymentMode: String(input.paymentMode || 'cash'),
    signAndSend: input.signAndSend !== false,
    certificatePassword:
      input.certificatePassword || process.env.SRI_CERTIFICATE_PASSWORD || '',
    customer: {
      ruc: String(input.customer.ruc).trim(),
      legalName: String(input.customer.legalName || '').trim(),
      email: String(input.customer.email || '').trim() || null,
      phone: String(input.customer.phone || '').trim() || null,
    },
    lines,
    total: computeInvoiceTotal(lines),
  };
}

export async function issueSriInvoice(input, options = {}) {
  const request = normalizeRequest(input);
  const client = await createCdpClient(options);
  try {
    const productTargetId = await getOrCreateTarget(
      client,
      (url) => url.includes('/catalogos/administracion/productos.html'),
      SRI_PRODUCTS_URL,
    );
    for (const line of request.lines) {
      await ensureProduct(client, productTargetId, line);
    }

    const invoiceTargetId = await getOrCreateTarget(
      client,
      (url) =>
        url.includes('/comprobantes/factura/Factura.html') ||
        url.includes('/pages/inicio.html'),
      SRI_START_URL,
    );
    await openInvoicePage(client, invoiceTargetId);
    const buyer = await fillInvoiceHeader(client, invoiceTargetId, request);
    for (const line of request.lines) {
      await addInvoiceLine(client, invoiceTargetId, line);
    }
    await addPayment(client, invoiceTargetId, request);
    const finalResult = await finalizeInvoice(client, invoiceTargetId, request);
    return {
      ok: true,
      targetId: invoiceTargetId,
      buyer,
      total: request.total,
      ...finalResult,
    };
  } finally {
    await client.close();
  }
}
