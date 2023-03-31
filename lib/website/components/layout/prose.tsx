import clsx from 'clsx';

export interface ProseProps {
  children: React.ReactNode;
  className?: string;
  [key: string]: any;
}

export function Prose({ children, className, ...props }: ProseProps) {
  return (
    <div className={clsx(className, 'prose dark:prose-invert')} {...props}>
      {children}
    </div>
  );
}
