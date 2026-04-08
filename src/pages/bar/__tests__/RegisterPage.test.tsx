import { QueryClient, QueryClientProvider } from "@tanstack/react-query";
import { render, screen, waitFor, within } from "@testing-library/react";
import userEvent from "@testing-library/user-event";
import { MemoryRouter, Route, Routes } from "react-router-dom";
import RegisterPage from "../Register";
import { vi } from "vitest";

const mockOpenRegister = vi.fn();
const mockCashDrop = vi.fn();
const mockCountRegister = vi.fn();
const mockCloseRegister = vi.fn();

vi.mock("../../../api/bar", () => ({
  openRegister: mockOpenRegister,
  cashDrop: mockCashDrop,
  countRegister: mockCountRegister,
  closeRegister: mockCloseRegister,
}));

function renderRegister(path = "/bar/register") {
  const queryClient = new QueryClient({
    defaultOptions: { queries: { retry: false } },
  });

  return render(
    <QueryClientProvider client={queryClient}>
      <MemoryRouter initialEntries={[path]}>
        <Routes>
          <Route path="/bar/register" element={<RegisterPage />} />
        </Routes>
      </MemoryRouter>
    </QueryClientProvider>,
  );
}

describe("RegisterPage", () => {
  beforeEach(() => {
    vi.clearAllMocks();
  });

  it("shows info when booking or station is missing", () => {
    renderRegister("/bar/register");
    expect(
      screen.getByText(/Proporciona `bookingId` y `stationId`/i),
    ).toBeInTheDocument();
  });

  it("walks through opening, drop, count and close actions", async () => {
    mockOpenRegister.mockResolvedValueOnce({ shiftId: 55 });
    mockCashDrop.mockResolvedValueOnce({});
    mockCountRegister.mockResolvedValueOnce({
      declaredCents: 10000,
      expectedCents: 9500,
      overShortCents: 500,
    });
    mockCloseRegister.mockResolvedValueOnce({});

    renderRegister("/bar/register?bookingId=10&stationId=20");

    const floatField = screen.getByLabelText("Fondo inicial ($)");
    await userEvent.clear(floatField);
    await userEvent.type(floatField, "50");
    await userEvent.click(screen.getByRole("button", { name: "Abrir" }));

    await waitFor(() =>
      expect(mockOpenRegister).toHaveBeenCalledWith({
        barEventId: 10,
        stationId: 20,
        openingFloatCents: 5000,
      }),
    );
    await waitFor(() =>
      expect(screen.getByText("Turno activo: #55")).toBeInTheDocument(),
    );

    const dropField = screen.getByLabelText("Depósito ($)");
    await userEvent.clear(dropField);
    await userEvent.type(dropField, "12.5");
    await userEvent.click(screen.getByRole("button", { name: "Registrar" }));

    await waitFor(() =>
      expect(mockCashDrop).toHaveBeenCalledWith({
        shiftId: 55,
        amountCents: 1250,
      }),
    );

    const hundredRow = screen.getByText("$100.00").parentElement;
    if (!hundredRow) {
      throw new Error("Row for $100.00 denomination not found");
    }
    const hundredInput = within(hundredRow).getByRole("spinbutton");
    await userEvent.clear(hundredInput);
    await userEvent.type(hundredInput, "1");

    await userEvent.click(
      screen.getByRole("button", { name: "Calcular diferencia" }),
    );

    await waitFor(() =>
      expect(mockCountRegister).toHaveBeenCalledWith({
        shiftId: 55,
        counts: [[10000, 1]],
      }),
    );

    await waitFor(() =>
      expect(
        screen.getByText(/Diferencia: 5\.00/i),
      ).toBeInTheDocument(),
    );

    await userEvent.click(
      screen.getByRole("button", { name: "Cerrar turno" }),
    );

    await waitFor(() =>
      expect(mockCloseRegister).toHaveBeenCalledWith({
        shiftIdClose: 55,
        declaredCloseCents: 10000,
      }),
    );
  });
});
