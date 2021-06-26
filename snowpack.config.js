/** @type {import("snowpack").SnowpackUserConfig } */
module.exports = {
  mount: {
    public: { url: '/', static: true },
    'lib/es6_global/src': { url: '/dist' },
    'node_modules/rescript-material-ui/lib/es6_global/src': { url: '/node_modules/rescript-material-ui/lib/es6_global/src' },
    'node_modules/rescript/lib/es6': { url: '/node_modules/rescript/lib/es6' },
    'node_modules/@rescript/react/lib/es6_global/src': { url: '/node_modules/@rescript/react/lib/es6_global/src' }
  },
  plugins: [
    '@snowpack/plugin-react-refresh',
    '@snowpack/plugin-dotenv'
  ],
  routes: [
    /* Enable an SPA Fallback in development: */
    {"match": "routes", "src": ".*", "dest": "/index.html"},
  ],
  optimize: {
    /* Example: Bundle your final build: */
    // "bundle": true,
  },
  packageOptions: {
    /* ... */
  },
  devOptions: {
    /* ... */
  },
  buildOptions: {
    /* ... */
  },
};
