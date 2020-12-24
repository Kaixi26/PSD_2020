package Services;

public class ServiceResult<TResult> {
    private final boolean success;
    private final TResult result;

    public ServiceResult(boolean success, TResult result) {
        this.success = false;
        this.result = result;
    }

    public boolean isSuccess() {
        return this.success;
    }

    public TResult getResult() {
        return this.result;
    }
}
