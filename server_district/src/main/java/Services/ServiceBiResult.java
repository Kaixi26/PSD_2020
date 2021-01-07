package Services;

public class ServiceBiResult<TResult, UResult> {
    private final boolean success;
    private final TResult result;
    private final UResult additionalResult;

    public ServiceBiResult(boolean success, TResult result, UResult additionalResult) {
        this.success = success;
        this.result = result;
        this.additionalResult = additionalResult;
    }


    public boolean isSuccess() {
        return this.success;
    }

    public TResult getResult() {
        return this.result;
    }

    public UResult getAdditionalResult() {
        return this.additionalResult;
    }
}
