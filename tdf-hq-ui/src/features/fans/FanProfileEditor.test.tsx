import { jest } from '@jest/globals';
import { useState, type RefObject } from 'react';
import { fireEvent, render, screen } from '@testing-library/react';
import type { FanProfileUpdate } from '../../api/types';
import { FanProfileEditor } from './FanProfileEditor';

const profileDraft: FanProfileUpdate = {
  fpuDisplayName: 'Ada Fan',
  fpuAvatarUrl: 'https://cdn.example.com/avatar.jpg',
  fpuFavoriteGenres: 'Soul, Indie',
  fpuBio: 'Always at the front row.',
  fpuCity: 'Quito',
};

function FanProfileEditorHarness({
  onSave = () => undefined,
}: {
  onSave?: () => void;
}) {
  const [draft, setDraft] = useState<FanProfileUpdate>(profileDraft);

  return (
    <FanProfileEditor
      avatarInputRef={{ current: null } as RefObject<HTMLInputElement>}
      displayNameFallback="Ada"
      profileDraft={draft}
      saving={false}
      setProfileDraft={setDraft}
      onAvatarFileChange={() => undefined}
      onSave={onSave}
    />
  );
}

describe('FanProfileEditor', () => {
  it('edits profile fields and calls save', () => {
    const onSave = jest.fn();

    render(<FanProfileEditorHarness onSave={onSave} />);

    const displayNameInput = screen.getByLabelText('Nombre público');
    fireEvent.change(displayNameInput, { target: { value: 'Ada Club' } });
    fireEvent.click(screen.getByRole('button', { name: 'Guardar perfil' }));

    expect(displayNameInput.value).toBe('Ada Club');
    expect(onSave).toHaveBeenCalledTimes(1);
  });

  it('clears the avatar URL from the draft', () => {
    render(<FanProfileEditorHarness />);

    fireEvent.click(screen.getByRole('button', { name: 'Quitar' }));

    expect(screen.getByLabelText('Avatar (pega enlace o sube)').value).toBe('');
    expect(screen.queryByRole('button', { name: 'Quitar' })).toBeNull();
  });
});
