type MDXComponents = any;

export function useMDXComponents(components: MDXComponents): MDXComponents {
  return {
    ...components,
  };
}
