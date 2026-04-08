import { QueryClient, QueryClientProvider } from "@tanstack/react-query";
import { render, screen, waitFor } from "@testing-library/react";
import userEvent from "@testing-library/user-event";
import { MemoryRouter, Route, Routes } from "react-router-dom";
import SellPage from "../Sell";
import { vi } from "vitest";

const mockGetMenu = vi.fn();
const mockPostSale = vi.fn();

vi.mock("../../../api/bar", () => ({
  getMenu: mockGetMenu,
  postSale: mockPostSale,
}));

function renderSellPage(path = "/bar/sell") {
  const queryClient = new QueryClient({
    defaultOptions: { queries: { retry: false } },
  });

  return render(
    <QueryClientProvider client={queryClient}>
      <MemoryRouter initialEntries={[path]}>
        <Routes>
          <Route path="/bar/sell" element={<SellPage />} />
        </Routes>
      </MemoryRouter>
    </QueryClientProvider>,
  );
}

describe("SellPage", () => {
  beforeEach(() => {
    vi.clearAllMocks();
  });

  it("shows a guidance alert when required query params are missing", () => {
    mockGetMenu.mockResolvedValueOnce([]);
    renderSellPage("/bar/sell");
    expect(
      screen.getByText(/Proporciona `bookingId`, `stationId` y `shiftId`/i),
    ).toBeInTheDocument();
  });

  it("allows adding items and submitting a sale", async () => {
    mockGetMenu.mockResolvedValueOnce([
      {
        miProductId: 101,
        miName: "Negroni",
        miCategory: "Cocktails",
        miUnitPriceCents: 1200,
      },
    ]);
    mockPostSale.mockResolvedValueOnce({});

    renderSellPage("/bar/sell?bookingId=1&stationId=2&shiftId=3");

    const productCard = await screen.findByText("Negroni");
    await userEvent.click(productCard);

    const tipField = screen.getByLabelText("Propina ($)");
    const discountField = screen.getByLabelText("Descuento ($)");

    await userEvent.clear(tipField);
    await userEvent.type(tipField, "2");
    await userEvent.clear(discountField);
    await userEvent.type(discountField, "1");

    await waitFor(() =>
      expect(screen.getByText("Subtotal: $12.00")).toBeInTheDocument(),
    );
    expect(screen.getByText("Total: $13.00")).toBeInTheDocument();

    await userEvent.click(screen.getByRole("button", { name: "Cobrar" }));

    await waitFor(() =>
      expect(mockPostSale).toHaveBeenCalledWith({
        bookingId: 1,
        stationId: 2,
        shiftId: 3,
        items: [
          { productId: 101, qty: 1, unitPriceCents: 1200 },
        ],
        discountCents: 100,
        taxCents: 0,
        tipCents: 200,
        paymentMethod: "PayCash",
      }),
    );

    await waitFor(() =>
      expect(
        screen.getByText(/Agrega productos del menú/i),
      ).toBeInTheDocument(),
    );
  });
});
