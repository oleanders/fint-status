import { defineConfig } from 'vite';

export default defineConfig(({ mode }) => ({
  build: { outDir: 'build' },
  server: mode === 'proxy' ? {
    host: 'localhost',
    proxy: {
      '/proxy/': {
        target: 'https://play-with-fint.felleskomponent.no',
        changeOrigin: true,
        rewrite: path => path.slice('/proxy'.length),
      },
    },
  } : undefined,
}));
