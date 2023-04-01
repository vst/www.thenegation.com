import clsx from 'clsx';

export interface ContentProps {
  children: React.ReactNode;
  className?: string;
  [key: string]: any;
}

export function Content({ children, className, ...props }: ContentProps) {
  return (
    <div className={clsx(['sm:px-8', className])} {...props}>
      <div className="mx-auto max-w-7xl lg:px-8">
        <div className="relative px-4 sm:px-8 lg:px-12">
          <div className="mx-auto max-w-2xl lg:max-w-5xl">{children}</div>
        </div>
      </div>
    </div>
  );
}
